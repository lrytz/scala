package scala.tools.nsc.backend.jvm

/* Copied and adapted from
 * https://github.com/apache/spark/blob/branch-2.3/core/src/main/scala/org/apache/spark/util/ClosureCleaner.scala,n
 * revision 51debf8b1f4d479bc7f81e2759ba28e526367d70.
 * Spark is licensed under Apache 2.
 */

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, FileInputStream, FileOutputStream, InputStream, ObjectInputStream, ObjectOutputStream, OutputStream}
import java.nio.channels.FileChannel

import scala.collection.mutable
import scala.collection.mutable.{Map, Set, Stack}
import scala.language.existentials
import scala.tools.asm.{ClassReader, ClassVisitor, MethodVisitor, Type}
import scala.tools.asm.Opcodes._

object Log {
  private val b = mutable.ArrayBuffer.empty[String]
  def log(s: String): Unit = { b += s }
  def get: Array[String] = b.toArray
  def reset(): Unit = b.clear()
}


class SparkException(message: String, cause: Throwable)
  extends Exception(message, cause) {

  def this(message: String) = this(message, null)
}

/**
  * A cleaner that renders closures serializable if they can be done so safely.
  */
private object ClosureCleaner {
  // Get an ASM class reader for a given class from the JAR that loaded it
  def getClassReader(cls: Class[_]): ClassReader = {
    // Copy data over, before delegating to ClassReader - else we can run out of open file handles.
    val className = cls.getName.replaceFirst("^.*\\.", "") + ".class"
    val resourceStream = cls.getResourceAsStream(className)
    if (resourceStream == null) {
      null
    } else {
      val baos = new ByteArrayOutputStream(128)
      Utils.copyStream(resourceStream, baos, true)
      new ClassReader(new ByteArrayInputStream(baos.toByteArray))
    }
  }

  // Check whether a class represents a Scala closure
  private def isClosure(cls: Class[_]): Boolean = {
    cls.getName.contains("$anonfun$")
  }

  // Get a list of the outer objects and their classes of a given closure object, obj;
  // the outer objects are defined as any closures that obj is nested within, plus
  // possibly the class that the outermost closure is in, if any. We stop searching
  // for outer objects beyond that because cloning the user's object is probably
  // not a good idea (whereas we can clone closure objects just fine since we
  // understand how all their fields are used).
  private def getOuterClassesAndObjects(obj: AnyRef): (List[Class[_]], List[AnyRef]) = {
    for (f <- obj.getClass.getDeclaredFields if f.getName == "$outer") {
      f.setAccessible(true)
      val outer = f.get(obj)
      // The outer pointer may be null if we have cleaned this closure before
      if (outer != null) {
        if (isClosure(f.getType)) {
          val recurRet = getOuterClassesAndObjects(outer)
          return (f.getType :: recurRet._1, outer :: recurRet._2)
        } else {
          return (f.getType :: Nil, outer :: Nil) // Stop at the first $outer that is not a closure
        }
      }
    }
    (Nil, Nil)
  }
  /**
    * Return a list of classes that represent closures enclosed in the given closure object.
    */
  private def getInnerClosureClasses(obj: AnyRef): List[Class[_]] = {
    val seen = Set[Class[_]](obj.getClass)
    val stack = Stack[Class[_]](obj.getClass)
    while (!stack.isEmpty) {
      val cr = getClassReader(stack.pop())
      if (cr != null) {
        val set = Set.empty[Class[_]]
        cr.accept(new InnerClosureFinder(set), 0)
        for (cls <- set -- seen) {
          seen += cls
          stack.push(cls)
        }
      }
    }
    (seen - obj.getClass).toList
  }

  /** Initializes the accessed fields for outer classes and their super classes. */
  private def initAccessedFields(
      accessedFields: Map[Class[_], Set[String]],
      outerClasses: Seq[Class[_]]): Unit = {
    for (cls <- outerClasses) {
      var currentClass = cls
      assert(currentClass != null, "The outer class can't be null.")

      while (currentClass != null) {
        accessedFields(currentClass) = Set.empty[String]
        currentClass = currentClass.getSuperclass()
      }
    }
  }

  /** Sets accessed fields for given class in clone object based on given object. */
  private def setAccessedFields(
      outerClass: Class[_],
      clone: AnyRef,
      obj: AnyRef,
      accessedFields: Map[Class[_], Set[String]]): Unit = {
    for (fieldName <- accessedFields(outerClass)) {
      val field = outerClass.getDeclaredField(fieldName)
      field.setAccessible(true)
      val value = field.get(obj)
      field.set(clone, value)
    }
  }

  /** Clones a given object and sets accessed fields in cloned object. */
  private def cloneAndSetFields(
      parent: AnyRef,
      obj: AnyRef,
      outerClass: Class[_],
      accessedFields: Map[Class[_], Set[String]]): AnyRef = {
    val clone = instantiateClass(outerClass, parent)

    var currentClass = outerClass
    assert(currentClass != null, "The outer class can't be null.")

    while (currentClass != null) {
      setAccessedFields(currentClass, clone, obj, accessedFields)
      currentClass = currentClass.getSuperclass()
    }

    clone
  }

  /**
    * Clean the given closure in place.
    *
    * More specifically, this renders the given closure serializable as long as it does not
    * explicitly reference unserializable objects.
    *
    * @param closure the closure to clean
    * @param checkSerializable whether to verify that the closure is serializable after cleaning
    * @param cleanTransitively whether to clean enclosing closures transitively
    */
  def clean(
      closure: AnyRef,
      checkSerializable: Boolean = true,
      cleanTransitively: Boolean = true): Unit = {
    clean(closure, checkSerializable, cleanTransitively, Map.empty)
  }

  /**
    * Helper method to clean the given closure in place.
    *
    * The mechanism is to traverse the hierarchy of enclosing closures and null out any
    * references along the way that are not actually used by the starting closure, but are
    * nevertheless included in the compiled anonymous classes. Note that it is unsafe to
    * simply mutate the enclosing closures in place, as other code paths may depend on them.
    * Instead, we clone each enclosing closure and set the parent pointers accordingly.
    *
    * By default, closures are cleaned transitively. This means we detect whether enclosing
    * objects are actually referenced by the starting one, either directly or transitively,
    * and, if not, sever these closures from the hierarchy. In other words, in addition to
    * nulling out unused field references, we also null out any parent pointers that refer
    * to enclosing objects not actually needed by the starting closure. We determine
    * transitivity by tracing through the tree of all methods ultimately invoked by the
    * inner closure and record all the fields referenced in the process.
    *
    * For instance, transitive cleaning is necessary in the following scenario:
    *
    *   class SomethingNotSerializable {
    *     def someValue = 1
    *     def scope(name: String)(body: => Unit) = body
    *     def someMethod(): Unit = scope("one") {
    *       def x = someValue
    *       def y = 2
    *       scope("two") { println(y + 1) }
    *     }
    *   }
    *
    * In this example, scope "two" is not serializable because it references scope "one", which
    * references SomethingNotSerializable. Note that, however, the body of scope "two" does not
    * actually depend on SomethingNotSerializable. This means we can safely null out the parent
    * pointer of a cloned scope "one" and set it the parent of scope "two", such that scope "two"
    * no longer references SomethingNotSerializable transitively.
    *
    * @param func the starting closure to clean
    * @param checkSerializable whether to verify that the closure is serializable after cleaning
    * @param cleanTransitively whether to clean enclosing closures transitively
    * @param accessedFields a map from a class to a set of its fields that are accessed by
    *                       the starting closure
    */
  private def clean(
      func: AnyRef,
      checkSerializable: Boolean,
      cleanTransitively: Boolean,
      accessedFields: Map[Class[_], Set[String]]): Unit = {

    assert(isClosure(func.getClass), s"Expected a closure; got ${func.getClass.getName}")

    // TODO: clean all inner closures first. This requires us to find the inner objects.
    // TODO: cache outerClasses / innerClasses / accessedFields

    if (func == null) {
      return
    }

    Log.log(s"+++ Cleaning closure $func (${func.getClass.getName}) +++")

    // A list of classes that represents closures enclosed in the given one
    val innerClasses = getInnerClosureClasses(func)

    // A list of enclosing objects and their respective classes, from innermost to outermost
    // An outer object at a given index is of type outer class at the same index
    val (outerClasses, outerObjects) = getOuterClassesAndObjects(func)

    // For logging purposes only
    val declaredFields = func.getClass.getDeclaredFields
    val declaredMethods = func.getClass.getDeclaredMethods

    {
      Log.log(s" + declared fields: ${declaredFields.size}")
      declaredFields.foreach { f => Log.log(s"     $f") }
      Log.log(s" + declared methods: ${declaredMethods.size}")
      declaredMethods.foreach { m => Log.log(s"     $m") }
      Log.log(s" + inner classes: ${innerClasses.size}")
      innerClasses.foreach { c => Log.log(s"     ${c.getName}") }
      Log.log(s" + outer classes: ${outerClasses.size}" )
      outerClasses.foreach { c => Log.log(s"     ${c.getName}") }
      Log.log(s" + outer objects: ${outerObjects.size}")
      outerObjects.foreach { o => Log.log(s"     $o") }
    }

    // Fail fast if we detect return statements in closures
    getClassReader(func.getClass).accept(new ReturnStatementFinder(), 0)

    // If accessed fields is not populated yet, we assume that
    // the closure we are trying to clean is the starting one
    if (accessedFields.isEmpty) {
      Log.log(" + populating accessed fields because this is the starting closure")
      // Initialize accessed fields with the outer classes first
      // This step is needed to associate the fields to the correct classes later
      initAccessedFields(accessedFields, outerClasses)

      // Populate accessed fields by visiting all fields and methods accessed by this and
      // all of its inner closures. If transitive cleaning is enabled, this may recursively
      // visits methods that belong to other classes in search of transitively referenced fields.
      for (cls <- func.getClass :: innerClasses) {
        getClassReader(cls).accept(new FieldAccessFinder(accessedFields, cleanTransitively), 0)
      }
    }

    Log.log(s" + fields accessed by starting closure: " + accessedFields.size)
    accessedFields.foreach { f => Log.log("     " + f) }

    // List of outer (class, object) pairs, ordered from outermost to innermost
    // Note that all outer objects but the outermost one (first one in this list) must be closures
    var outerPairs: List[(Class[_], AnyRef)] = outerClasses.zip(outerObjects).reverse
    var parent: AnyRef = null
    if (outerPairs.nonEmpty) {
      val (outermostClass, outermostObject) = outerPairs.head
      if (isClosure(outermostClass)) {
        Log.log(s" + outermost object is a closure, so we clone it: ${outerPairs.head}")
      } else if (outermostClass.getName.startsWith("$line")) {
        // SPARK-14558: if the outermost object is a REPL line object, we should clone
        // and clean it as it may carray a lot of unnecessary information,
        // e.g. hadoop conf, spark conf, etc.
        Log.log(s" + outermost object is a REPL line object, so we clone it: ${outerPairs.head}")
      } else {
        // The closure is ultimately nested inside a class; keep the object of that
        // class without cloning it since we don't want to clone the user's objects.
        // Note that we still need to keep around the outermost object itself because
        // we need it to clone its child closure later (see below).
        Log.log(" + outermost object is not a closure or REPL line object," +
          "so do not clone it: " +  outerPairs.head)
        parent = outermostObject // e.g. SparkContext
        outerPairs = outerPairs.tail
      }
    } else {
      Log.log(" + there are no enclosing objects!")
    }

    // Clone the closure objects themselves, nulling out any fields that are not
    // used in the closure we're working on or any of its inner closures.
    for ((cls, obj) <- outerPairs) {
      Log.log(s" + cloning the object $obj of class ${cls.getName}")
      // We null out these unused references by cloning each object and then filling in all
      // required fields from the original object. We need the parent here because the Java
      // language specification requires the first constructor parameter of any closure to be
      // its enclosing object.
      val clone = cloneAndSetFields(parent, obj, cls, accessedFields)

      // If transitive cleaning is enabled, we recursively clean any enclosing closure using
      // the already populated accessed fields map of the starting closure
      if (cleanTransitively && isClosure(clone.getClass)) {
        Log.log(s" + cleaning cloned closure $clone recursively (${cls.getName})")
        // No need to check serializable here for the outer closures because we're
        // only interested in the serializability of the starting closure
        clean(clone, checkSerializable = false, cleanTransitively, accessedFields)
      }
      parent = clone
    }

    // Update the parent pointer ($outer) of this closure
    if (parent != null) {
      val field = func.getClass.getDeclaredField("$outer")
      field.setAccessible(true)
      // If the starting closure doesn't actually need our enclosing object, then just null it out
      if (accessedFields.contains(func.getClass) &&
        !accessedFields(func.getClass).contains("$outer")) {
        Log.log(s" + the starting closure doesn't actually need $parent, so we null it out")
        field.set(func, null)
      } else {
        // Update this closure's parent pointer to point to our enclosing object,
        // which could either be a cloned closure or the original user object
        field.set(func, parent)
      }
    }

    Log.log(s" +++ closure $func (${func.getClass.getName}) is now cleaned +++")

    if (checkSerializable) {
      ensureSerializable(func)
    }
  }

  private def ensureSerializable(func: AnyRef) {
    try Utils.serializeDeserialize(func)
    catch {
      case ex: Exception => throw new SparkException("Task not serializable", ex)
    }
  }

  private def instantiateClass(
      cls: Class[_],
      enclosingObject: AnyRef): AnyRef = {
    // Use reflection to instantiate object without calling constructor
    val rf = sun.reflect.ReflectionFactory.getReflectionFactory()
    val parentCtor = classOf[java.lang.Object].getDeclaredConstructor()
    val newCtor = rf.newConstructorForSerialization(cls, parentCtor)
    val obj = newCtor.newInstance().asInstanceOf[AnyRef]
    if (enclosingObject != null) {
      val field = cls.getDeclaredField("$outer")
      field.setAccessible(true)
      field.set(obj, enclosingObject)
    }
    obj
  }
}

private class ReturnStatementFinder(targetMethodName: Option[String] = None)
  extends ClassVisitor(ASM6) {
  override def visitMethod(access: Int, name: String, desc: String,
      sig: String, exceptions: Array[String]): MethodVisitor = {

    // $anonfun$ covers Java 8 lambdas
    if (name.contains("apply") || name.contains("$anonfun$")) {
      // A method with suffix "$adapted" will be generated in cases like
      // { _:Int => return; Seq()} but not { _:Int => return; true}
      // closure passed is $anonfun$t$1$adapted while actual code resides in $anonfun$s$1
      // visitor will see only $anonfun$s$1$adapted, so we remove the suffix, see
      // https://github.com/scala/scala-dev/issues/109
      val isTargetMethod = targetMethodName.isEmpty ||
        name == targetMethodName.get || name == targetMethodName.get.stripSuffix("$adapted")

      new MethodVisitor(ASM6) {
        override def visitTypeInsn(op: Int, tp: String) {
          if (op == NEW && tp.contains("scala/runtime/NonLocalReturnControl") && isTargetMethod) {
            throw new SparkException("Return statements aren't allowed in Spark closures")
          }
        }
      }
    } else {
      new MethodVisitor(ASM6) {}
    }
  }
}

/** Helper class to identify a method. */
private case class MethodIdentifier[T](cls: Class[T], name: String, desc: String)

/**
  * Find the fields accessed by a given class.
  *
  * The resulting fields are stored in the mutable map passed in through the constructor.
  * This map is assumed to have its keys already populated with the classes of interest.
  *
  * @param fields the mutable map that stores the fields to return
  * @param findTransitively if true, find fields indirectly referenced through method calls
  * @param specificMethod if not empty, visit only this specific method
  * @param visitedMethods a set of visited methods to avoid cycles
  */
private class FieldAccessFinder(
    fields: Map[Class[_], Set[String]],
    findTransitively: Boolean,
    specificMethod: Option[MethodIdentifier[_]] = None,
    visitedMethods: Set[MethodIdentifier[_]] = Set.empty)
  extends ClassVisitor(ASM6) {

  override def visitMethod(
      access: Int,
      name: String,
      desc: String,
      sig: String,
      exceptions: Array[String]): MethodVisitor = {

    // If we are told to visit only a certain method and this is not the one, ignore it
    if (specificMethod.isDefined &&
      (specificMethod.get.name != name || specificMethod.get.desc != desc)) {
      return null
    }

    new MethodVisitor(ASM6) {
      override def visitFieldInsn(op: Int, owner: String, name: String, desc: String) {
        if (op == GETFIELD) {
          for (cl <- fields.keys if cl.getName == owner.replace('/', '.')) {
            fields(cl) += name
          }
        }
      }

      override def visitMethodInsn(
          op: Int, owner: String, name: String, desc: String, itf: Boolean) {
        for (cl <- fields.keys if cl.getName == owner.replace('/', '.')) {
          // Check for calls a getter method for a variable in an interpreter wrapper object.
          // This means that the corresponding field will be accessed, so we should save it.
          if (op == INVOKEVIRTUAL && owner.endsWith("$iwC") && !name.endsWith("$outer")) {
            fields(cl) += name
          }
          // Optionally visit other methods to find fields that are transitively referenced
          if (findTransitively) {
            val m = MethodIdentifier(cl, name, desc)
            if (!visitedMethods.contains(m)) {
              // Keep track of visited methods to avoid potential infinite cycles
              visitedMethods += m

              var currentClass = cl
              assert(currentClass != null, "The outer class can't be null.")

              while (currentClass != null) {
                ClosureCleaner.getClassReader(currentClass).accept(
                  new FieldAccessFinder(fields, findTransitively, Some(m), visitedMethods), 0)
                currentClass = currentClass.getSuperclass()
              }
            }
          }
        }
      }
    }
  }
}

private class InnerClosureFinder(output: Set[Class[_]]) extends ClassVisitor(ASM6) {
  var myName: String = null

  // TODO: Recursively find inner closures that we indirectly reference, e.g.
  //   val closure1 = () = { () => 1 }
  //   val closure2 = () => { (1 to 5).map(closure1) }
  // The second closure technically has two inner closures, but this finder only finds one

  override def visit(version: Int, access: Int, name: String, sig: String,
      superName: String, interfaces: Array[String]) {
    myName = name
  }

  override def visitMethod(access: Int, name: String, desc: String,
      sig: String, exceptions: Array[String]): MethodVisitor = {
    new MethodVisitor(ASM6) {
      override def visitMethodInsn(
          op: Int, owner: String, name: String, desc: String, itf: Boolean) {
        val argTypes = Type.getArgumentTypes(desc)
        if (op == INVOKESPECIAL && name == "<init>" && argTypes.length > 0
          && argTypes(0).toString.startsWith("L") // is it an object?
          && argTypes(0).getInternalName == myName) {
          // scalastyle:off classforname
          output += Class.forName(
            owner.replace('/', '.'),
            false,
            Thread.currentThread.getContextClassLoader)
          // scalastyle:on classforname
        }
      }
    }
  }
}

object Utils {
  def copyFileStreamNIO(
      input: FileChannel,
      output: FileChannel,
      startPosition: Long,
      bytesToCopy: Long): Unit = {
    val initialPos = output.position()
    var count = 0L
    // In case transferTo method transferred less data than we have required.
    while (count < bytesToCopy) {
      count += input.transferTo(count + startPosition, bytesToCopy - count, output)
    }
    assert(count == bytesToCopy,
      s"request to copy $bytesToCopy bytes, but actually copied $count bytes.")

    // Check the position after transferTo loop to see if it is in the right position and
    // give user information if not.
    // Position will not be increased to the expected length after calling transferTo in
    // kernel version 2.6.32, this issue can be seen in
    // https://bugs.openjdk.java.net/browse/JDK-7052359
    // This will lead to stream corruption issue when using sort-based shuffle (SPARK-3948).
    val finalPos = output.position()
    val expectedPos = initialPos + bytesToCopy
    assert(finalPos == expectedPos,
      s"""
         |Current position $finalPos do not equal to expected position $expectedPos
         |after transferTo, please check your kernel version to see if it is 2.6.32,
         |this is a kernel bug which will lead to unexpected behavior when using transferTo.
         |You can set spark.file.transferTo = false to disable this NIO feature.
           """.stripMargin)
  }

  /**
    * Copy all data from an InputStream to an OutputStream. NIO way of file stream to file stream
    * copying is disabled by default unless explicitly set transferToEnabled as true,
    * the parameter transferToEnabled should be configured by spark.file.transferTo = [true|false].
    */
  def copyStream(
      in: InputStream,
      out: OutputStream,
      closeStreams: Boolean = false,
      transferToEnabled: Boolean = false): Long = {
    try {
      if (in.isInstanceOf[FileInputStream] && out.isInstanceOf[FileOutputStream]
        && transferToEnabled) {
        // When both streams are File stream, use transferTo to improve copy performance.
        val inChannel = in.asInstanceOf[FileInputStream].getChannel()
        val outChannel = out.asInstanceOf[FileOutputStream].getChannel()
        val size = inChannel.size()
        copyFileStreamNIO(inChannel, outChannel, 0, size)
        size
      } else {
        var count = 0L
        val buf = new Array[Byte](8192)
        var n = 0
        while (n != -1) {
          n = in.read(buf)
          if (n != -1) {
            out.write(buf, 0, n)
            count += n
          }
        }
        count
      }
    } finally {
      if (closeStreams) {
        try {
          in.close()
        } finally {
          out.close()
        }
      }
    }
  }

  def serializeDeserialize[T <: AnyRef](obj: T): T = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}
