/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tools.nsc
package backend.jvm

import scala.reflect.internal.util.Statistics

// Enable with `-Ystatistics:jvm`
trait BackendStats {
  self: Statistics =>

  val bcodeTimer      = newTimer("time in backend", "jvm")
  val bcodeInitTimer  = newSubTimer("bcode initialization", bcodeTimer)
  val bcodeGenStat    = newSubTimer("code generation", bcodeTimer)
  val callGraphBuildTimer = newSubTimer("call graph building", bcodeTimer)
  val inlinerTimer    = newSubTimer("inlining", bcodeTimer)
  val callGraphRefreshTimer = newTimer("call graph refreshing (part of inlining)", "jvm")
  val closureOptTimer = newSubTimer("closure optimizer", bcodeTimer)

//  val methodOptTimer  = newSubTimer("intra-method optimizations", bcodeTimer)
  val nullnessOptTimer = newSubTimer("nulless opts", bcodeTimer)
  val dceTimer        = newSubTimer("dce", bcodeTimer)
  val boxUnboxTimer   = newSubTimer("box-unbox", bcodeTimer)
  val copyPropTimer   = newSubTimer("copy prop", bcodeTimer)
  val staleStoresTimer = newSubTimer("stale stores", bcodeTimer)
  val redundantCastsTimer = newSubTimer("redundant casts", bcodeTimer)
  val pushPopTimer     = newSubTimer("push-pop", bcodeTimer)

  val bcodeWriteTimer = newSubTimer("classfile writing", bcodeTimer)
}
