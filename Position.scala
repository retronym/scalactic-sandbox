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


import scala.reflect.macros.Context
import language.experimental.macros
/**
 * Helper class for Position macro. (Will be removed from the public API if possible in a subsequent 3.0.0-RCx release.)
 */
object Position {

  private[scalactic] lazy val showScalacticFillFilePathnames: Boolean = {
    val value = System.getenv("SCALACTIC_FILL_FILE_PATHNAMES")
    value != null && value == "yes"
  }

  /**
   * Helper method for Position macro.
   */
  def genPosition(context: Context) = {
    import context.universe._

    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Select(
                Select(
                  Ident(newTermName("_root_")),
                  newTermName("org")
                ),
              newTermName("scalactic")
              ),
              newTermName("source")
            ),
            newTermName("Position")
          ),
          newTermName("apply")
        ),
        List(
          Literal(Constant(context.enclosingPosition.source.file.name)),
          Literal(if (showScalacticFillFilePathnames) Constant(context.enclosingPosition.source.path) else Constant("")),
          Literal(Constant(context.enclosingPosition.line))
        )
      )
    )
  }
  implicit def here: Position = macro genPosition
  def apply(a: Any, b: Any, c: Any): Position = new Position
}
class Position {

}