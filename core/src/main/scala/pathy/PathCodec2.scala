/*
 * Copyright 2014 - 2015 SlamData Inc.
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

package pathy

sealed abstract class PathCodec2 {
  /*
  def parsePath[Z](
    rf: RelFile[Unsandboxed] => Z,
    af: AbsFile[Unsandboxed] => Z,
    rd: RelDir[Unsandboxed] => Z,
    ad: AbsDir[Unsandboxed] => Z
  ): String => Z =
  */
}

object PathCodec2 {
  import Path2._

/*
parse absolute directly, make sandboxing optional for relative?
or just ditch it alltogether?
*/

  final class UnsandboxedPath[T <: Typ] private[PathCodec2] (
    private val rp: RPath[T]
  ) extends AnyVal {
    def sandbox[B <: Base](d: Path2[B, Dir]): Option[RPath[T]] =
      ???
  }
}
