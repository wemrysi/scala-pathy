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
import scalaz.syntax.equal._

sealed abstract class RelativeTo[B <: Base] {
  type Out[T <: Typ]
  def relativeTo[T <: Typ](p: Path2[B, T], d: Path2[B, Dir]): Option[Out[T]]
}

object RelativeTo {
  implicit val abs: RelativeTo[Abs] = new RelativeTo[Abs] {
    type Out[T <: Typ] = LPath[T]

    def relativeTo[T <: Typ](p: APath[T], d: ADir): Option[LPath[T]] = {
      def go(d1: ADir, d2: ADir): Option[LDir] =
        if (d1 === d2)
          some(cur)
        else d1 match {
          case DirIn(pp, n)  => go(pp, d2) map (append(_, dir1(n)))
          case _             => none
        }

      p match {
        case FileIn(pp, n)    => go(pp, d) map (append(_, file1(n)))
        case dp @ DirIn(_, _) => go(dp, d)
        case dp @ Root        => go(dp, d)
      }
    }
  }

  implicit val loc: RelativeTo[Loc] = new RelativeTo[Loc] {
    type Out[T <: Typ] = LPath[T]

    def relativeTo[T <: Typ](p: LPath[T], d: LDir): Option[LPath[T]] = {
      def go(d1: LDir, d2: LDir): Option[LDir] =
        if (d1 === d2)
          some(cur)
        else d1 match {
          case DirIn(pp, n)  => go(pp, d2) map (append(_, dir1(n)))
          case _             => none
        }

      p match {
        case FileIn(pp, n)    => go(pp, d) map (append(_, file1(n)))
        case dp @ DirIn(_, _) => go(dp, d)
        case dp @ Current     => go(dp, d)
      }
    }
  }
/*
  implicit val esc: RelativeTo[Esc] = new RelativeTo[Esc] {
    type Out[T <: Typ] = Path2[Esc, T] \/ Path2[Loc, T]

    def relativeTo[T <: Typ](p: LPath[T], d: LDir): Path2[Esc, T] \/ LPath[T] = {
      def go(d1: LDir, d2: LDir): Option[LDir] =
        if (d1 === d2)
          some(cur)
        else d1 match {
          case DirIn(pp, n)  => go(pp, d2) map (append(_, dir1(n)))
          case _             => none
        }

      p match {
        case FileIn(pp, n)    => go(pp, d) map (append(_, file1(n)))
        case dp @ DirIn(_, _) => go(dp, d)
        case dp @ Current     => go(dp, d)
      }
    }
  }
*/
}

