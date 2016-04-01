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

import Path2._

import scalaz.std.option._

sealed abstract class Sandboxed[B <: Base] {
  def sandbox[T <: Typ](esc: Path2[Esc, T], d: Path2[B, Dir]): Option[Path2[B, T]]
}

object Sandboxed {
  implicit val abs: Sandboxed[Abs] = new Sandboxed[Abs] {
    def sandbox[T <: Typ](
      esc: Path2[Esc, T],
      d: Path2[Abs, Dir]
    ): Option[Path2[Abs, T]] = esc match {
      case FileIn(ep, n) =>
        sandbox(ep, d) map (_ / file1(n))
      case DirIn(ep, n) =>
        sandbox(ep, d) map (_ / dir1(n))
      case ParentIn(ep) => d match {
        case DirIn(ap, _) =>
          refineRel(ep).fold(
            e => sandbox(e, ap),
            l => some(ap / l))
        case Root =>
          none
      }
    }
  }

  implicit val loc: Sandboxed[Loc] = new Sandboxed[Loc] {
    def sandbox[T <: Typ](
      esc: Path2[Esc, T],
      d: Path2[Loc, Dir]
    ): Option[Path2[Loc, T]] = esc match {
      case FileIn(ep, n) =>
        sandbox(ep, d) map (_ / file1(n))
      case DirIn(ep, n) =>
        sandbox(ep, d) map (_ / dir1(n))
      case ParentIn(ep) => d match {
        case DirIn(lp, _) =>
          refineRel(ep).fold(
            e => sandbox(e, lp),
            l => some(lp / l))
        case Current =>
          none
      }
    }
  }
}
