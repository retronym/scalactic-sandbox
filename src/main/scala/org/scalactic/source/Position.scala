/*
 * Copyright 2001-2016 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalactic.source


import java.util

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
/**
 * Helper class for Position macro. (Will be removed from the public API if possible in a subsequent 3.0.0-RCx release.)
 */
object Position {

  private[scalactic] lazy val showScalacticFillFilePathnames: Boolean = {
    val value = System.getenv("SCALACTIC_FILL_FILE_PATHNAMES")
    value != null && value == "yes"
  }
  class PositionImpl(val universe: scala.reflect.api.Universe) {

    private val PositionModule = universe.rootMirror.staticModule("org.scalactic.source.Position")
    private val Position_apply = PositionModule.info.decl(universe.TermName("apply"))

    def apply(context: Context): context.Tree = {
      import context.universe._
      val args = List(
        Literal(Constant(context.enclosingPosition.source.file.name)),
        Literal(if (showScalacticFillFilePathnames) Constant(context.enclosingPosition.source.path) else Constant("")),
        Literal(Constant(context.enclosingPosition.line))
      )
      internal.gen.mkMethodCall(Position_apply.asInstanceOf[Symbol], args)
    }
  }
  private var cache = new java.lang.ref.WeakReference[PositionImpl](null)
  /**
   * Helper method for Position macro.
   */
  def genPosition(context: Context): context.Tree = {
    var impl = cache.get()
    if (impl == null || impl.universe != context.universe) {
      impl = new PositionImpl(context.universe)
      cache = new java.lang.ref.WeakReference[PositionImpl](impl)
    }
    impl.apply(context)
  }

  implicit def here: Position = macro genPosition
  def apply(a: Any, b: Any, c: Any): Position = new Position
}
class Position {

}