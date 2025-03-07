package scala;

import net.bytebuddy.ByteBuddy;
import net.bytebuddy.agent.ByteBuddyAgent;
import net.bytebuddy.asm.Advice;
import net.bytebuddy.dynamic.loading.ClassReloadingStrategy;
import net.bytebuddy.matcher.ElementMatchers;

import java.io.ObjectStreamClass;
import java.io.ObjectStreamField;
import java.lang.Long;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;


/**
 * Instrument `ObjectStream.FieldReflector.setObjFieldValues` to
 *   - avoid the type test
 *   - don't set the field value if it's a serialization proxy (has a readResolve)
 *   - remember where to store the `readResolve` result in the `delayed` map
 *
 * Instrument `ObjectStreamClass.invokeReadResolve` to
 *   - set the resulting object in all places where the proxy was skipped
 *
 * Doesn't fix deserialization of classes with custom `readObject` implementations. When calling
 * `ObjectInputStream.readObject` in a custom implementation, that may return a serialization proxy.
 */
public class ByteBuddyDynamicPatch {
    // SerializationProxyInstance -> List( List(targetObject, fieldIndex), List(targetObject, fieldIndex), ...)
    public static final java.util.Map<Object, List<List<Object>>> delayed = new java.util.HashMap<>();

    // Locate the private inner class via reflection
    private static Class<?> getFieldReflectorClass() throws Exception {
        for (Class<?> innerClass : ObjectStreamClass.class.getDeclaredClasses()) {
            if (innerClass.getSimpleName().equals("FieldReflector")) {
                return innerClass;
            }
        }
        throw new ClassNotFoundException("FieldReflector not found");
    }
    public static void install() throws Exception {
        ByteBuddyAgent.install();

        Class<?> fieldReflectorClass = getFieldReflectorClass();

        new ByteBuddy()
                .redefine(fieldReflectorClass)
                .visit(Advice.to(MethodInterceptor.class)
                        .on(ElementMatchers.named("setObjFieldValues")
                                .and(ElementMatchers.takesArguments(Object.class, Object[].class, boolean.class))))
                .make()
                .load(fieldReflectorClass.getClassLoader(), ClassReloadingStrategy.fromInstalledAgent());

        new ByteBuddy()
                .redefine(Class.forName("java.io.ObjectStreamClass"))
                .visit(Advice.to(ReadResolveInterceptor.class)
                        .on(ElementMatchers.named("invokeReadResolve")))
                .make()
                .load(ClassLoader.getSystemClassLoader(), ClassReloadingStrategy.fromInstalledAgent());
    }

    public static class ReadResolveInterceptor {
        @Advice.OnMethodExit
        static void onExit(@Advice.Argument(0) Object proxy, @Advice.Return Object returnValue) throws ClassNotFoundException, NoSuchFieldException, IllegalAccessException {
            // `ByteBuddyDynamicPatch.delayed` fails with ClassNotFound, didn't figure out the classloader stuff that would be needed
            java.util.Map<Object, List<List<Object>>> delayed = (java.util.Map<Object, List<List<Object>>>) Class.forName("scala.ByteBuddyDynamicPatch", true, ClassLoader.getSystemClassLoader()).getField("delayed").get(null);

            sun.misc.Unsafe unsafe = null;
            for (Field field : sun.misc.Unsafe.class.getDeclaredFields()) {
                if (field.getType() == sun.misc.Unsafe.class) {
                    field.setAccessible(true);
                    unsafe = (sun.misc.Unsafe) field.get(null);
                    break;
                }
            }

            List<List<Object>> argss = delayed.remove(proxy);
            if (argss != null) for (List<Object> args : argss) {
                Object obj = args.get(0);
                long key = (Long) args.get(1);
                // System.out.println("exit: " + obj + " - " + args + " - " + returnValue);
                unsafe.putObject(obj, key, returnValue);
            }
        }
    }

    // Interceptor to replace method logic
    public static class MethodInterceptor {

        // inlined because calls to it fail with ClassNotFound
        public static Object getField(Object o, String s) throws NoSuchFieldException, IllegalAccessException {
            Field f = o.getClass().getDeclaredField("fields");
            f.setAccessible(true);
            return f.get(o);
        }

        @Advice.OnMethodEnter(skipOn = Advice.OnNonDefaultValue.class)
        static boolean onEnter(@Advice.This Object instance,
                               @Advice.Argument(0) Object obj,
                               @Advice.Argument(1) Object[] vals,
                               @Advice.Argument(2) boolean dryRun) throws NoSuchFieldException, IllegalAccessException, ClassNotFoundException {

            sun.misc.Unsafe unsafe = null;
            for (Field field : sun.misc.Unsafe.class.getDeclaredFields()) {
                if (field.getType() == sun.misc.Unsafe.class) {
                    field.setAccessible(true);
                    unsafe = (sun.misc.Unsafe) field.get(null);
                    break;
                }
            }

            java.util.Map<Object, List<List<Object>>> delayed = (java.util.Map<Object, List<List<Object>>>) Class.forName("scala.ByteBuddyDynamicPatch", true, ClassLoader.getSystemClassLoader()).getField("delayed").get(null);


            Field f1 = instance.getClass().getDeclaredField("numPrimFields");
            f1.setAccessible(true);
            int numPrimFields = f1.getInt(instance);
            Field f2 = instance.getClass().getDeclaredField("fields");
            f2.setAccessible(true);
            ObjectStreamField[] fields = (ObjectStreamField[]) f2.get(instance);
            Field f3 = instance.getClass().getDeclaredField("writeKeys");
            f3.setAccessible(true);
            long[] writeKeys = (long[]) f3.get(instance);
            Field f4 = instance.getClass().getDeclaredField("typeCodes");
            f4.setAccessible(true);
            char[] typeCodes = (char[]) f4.get(instance);
            Field f5 = instance.getClass().getDeclaredField("offsets");
            f5.setAccessible(true);
            int[] offsets = (int[]) f5.get(instance);
            Field f6 = instance.getClass().getDeclaredField("types");
            f6.setAccessible(true);
            Class<?>[] types = (Class<?>[]) f6.get(instance);


            if (obj == null) {
                throw new NullPointerException();
            }


            for (int i = numPrimFields; i < fields.length; i++) {
                long key = writeKeys[i];
                if (key == -1) {
                    continue;           // discard value
                }
                if (typeCodes[i] == 'L' || typeCodes[i] == '[') {
                    Object val = vals[offsets[i]];
                    /* skip the check
                    if (val != null &&
                            !types[i - numPrimFields].isInstance(val)) {
                        Field f7 = ((Object) fields[i]).getClass().getDeclaredField("field");
                        f7.setAccessible(true);
                        Field f = (Field) f7.get(fields[i]);
                        throw new ClassCastException(
                                "cannot assign instance of " +
                                        val.getClass().getName() + " to field " +
                                        f.getDeclaringClass().getName() + "." +
                                        f.getName() + " of type " +
                                        f.getType().getName() + " in instance of " +
                                        obj.getClass().getName());
                    }
                    */
                    if (!dryRun) {
                        try {
                            val.getClass().getMethod("readResolve");
                            System.out.println("skip " + obj);
                            List<List<Object>> argss = delayed.get(key);
                            if (argss == null) argss = new ArrayList<>();
                            argss.add(List.of(obj, key));
                            delayed.put(val, argss);
                            val = null;
                        } catch (NoSuchMethodException e) {
                            // nothing
                        }
                        unsafe.putObject(obj, key, val);
                    }
                } else {
                    throw new InternalError();
                }
            }

            return true; // skip original impl
        }
    }

}
