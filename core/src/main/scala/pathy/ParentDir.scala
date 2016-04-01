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

import scalaz.\/
import scalaz.syntax.either._

sealed abstract class ParentDir[B <: Base] {
  type Out
  def parentDir(p: Path2[B, _]): Out
}

object ParentDir {
  type Aux[B <: Base, Out0] = ParentDir[B] { type Out = Out0 }

  implicit val abs: Aux[Abs, ADir] = new ParentDir[Abs] {
    type Out = ADir

    def parentDir(p: Path2[Abs, _]): ADir = p match {
      case FileIn(d, _) => d
      case DirIn(d, _)  => d
      case Root         => root
    }
  }

  implicit val loc: Aux[Loc, Path2[_ <: Rel, Dir]] = new ParentDir[Loc] {
    type Out = Path2[_ <: Rel, Dir]

    def parentDir(p: Path2[Loc, _]): Path2[_ <: Rel, Dir] = p match {
      case FileIn(d, _) => d
      case DirIn(d, _)  => d
      case Current      => ParentIn(cur)
    }
  }

  implicit val esc: Aux[Esc, Path2[Esc, Dir]] = new ParentDir[Esc] {
    type Out = Path2[Esc, Dir]

    def parentDir(p: Path2[Esc, _]): Path2[Esc, Dir] = p match {
      case FileIn(d, _) => d
      case DirIn(d, _)  => d
      case ParentIn(p1) =>
        refineRel(p1)
          .fold[Path2[Esc, Dir] \/ LDir](
            _.parentDir.left,
            l => refineRel(l.parentDir))
          .fold(ParentIn(_), ParentIn(_))
    }
  }
}
