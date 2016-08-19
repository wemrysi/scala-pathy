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

import scala.Function.const

import scala.annotation.tailrec
import scalaz._, Tags.Conjunction
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.std.string._
import scalaz.syntax.either._
import scalaz.syntax.monoid._
import scalaz.syntax.show._

sealed abstract class Path2[B <: Path2.Base, T <: Path2.Typ] {
  override def toString = this.shows
}

// TODO: Lenses (in another module)
// TODO: How many aliases do we want, another set for `Esc` varieties?
//       What about the version that fixes the type?
object Path2 {
  sealed abstract class Base
  sealed abstract class Abs extends Base
  sealed abstract class Rel extends Base
  sealed abstract class Loc extends Rel
  sealed abstract class Esc extends Rel

  sealed abstract class Typ
  sealed abstract class File extends Typ
  sealed abstract class Dir extends Typ

  final case class FileName(value: String, extension: Option[String]) {
    def dropExtension: FileName =
      copy(extension = None)

    def modifyExtension(f: String => String): FileName =
      copy(extension = extension.map(f))
  }

  object FileName {
    def apply(fn: String): FileName = {
      val idx = fn.lastIndexOf(".")
      if (idx == -1)
        FileName(fn, None)
      else
        FileName(fn.substring(0, idx), Some(fn.substring(idx + 1, fn.length)))
    }
  }

  final case class DirName(value: String) extends AnyVal

  private[pathy] final case object Root
    extends Path2[Abs, Dir]
  private[pathy] final case object Current
    extends Path2[Loc, Dir]
  private[pathy] final case class ParentIn[B <: Rel](parent: Path2[B, Dir])
    extends Path2[Esc, Dir]
  private[pathy] final case class DirIn[B <: Base](parent: Path2[B, Dir], name: DirName)
    extends Path2[B, Dir]
  private[pathy] final case class FileIn[B <: Base](parent: Path2[B, Dir], name: FileName)
    extends Path2[B, File]

  //--- Syntax ---

  final implicit class PathOps[B <: Base, T <: Typ](val p: Path2[B, T]) extends AnyVal {
    def depth: Int =
      Path2.depth(p)

    def foldMap1[A: Semigroup](
      fileIn: FileName => A,
      dirIn: DirName => A,
      parentIn: => A,
      cur: => A,
      root: => A
    ): A =
      Path2.foldMap1(fileIn, dirIn, parentIn, cur, root, p)

    def foldMap[A: Monoid](
      fileIn: FileName => A,
      dirIn: DirName => A,
      parentIn: => A
    ): A =
      Path2.foldMap(fileIn, dirIn, parentIn, p)

    def isAbsolute: Boolean =
      Path2.isAbsolute(p)

    def isRelative: Boolean =
      Path2.isRelative(p)

    def maybeDir: Option[Path2[B, Dir]] =
      Path2.maybeDir(p)

    def maybeFile: Option[Path2[B, File]] =
      Path2.maybeFile(p)

    def parentDir(implicit B: ParentDir[B]): B.Out =
      Path2.parentDir(p)

    def peel: Option[(Path2[B, Dir], DirName \/ FileName)] =
      Path2.peel(p)

    def refineType: Path2[B, Dir] \/ Path2[B, File] =
      Path2.refineType(p)

    def relativeTo(d: Path2[B, Dir])(implicit R: RelativeTo[B]): Option[R.Out[T]] =
      Path2.relativeTo(p, d)
  }

  final implicit class FileOps[B <: Base](val p: Path2[B, File]) extends AnyVal {
    def name: FileName =
      Path2.fileName(p)

    def rename(f: FileName => FileName): Path2[B, File] =
      Path2.renameFile(f)(p)
  }

  final implicit class DirOps[B <: Base](val p: Path2[B, Dir]) extends AnyVal {
    def /[T <: Typ](r: LPath[T]): Path2[B, T] =
      Path2.append(p, r)

    def /(n: String): Path2[B, Dir] =
      p / dir(n)

    def />(n: String): Path2[B, File] =
      p / file(n)

    def name: Option[DirName] =
      Path2.dirName(p)

    def rename(f: DirName => DirName): Path2[B, Dir] =
      Path2.renameDir(f)(p)
  }

  final implicit class AbsOps[T <: Typ](val p: Path2[Abs, T]) extends AnyVal {
    def asRelative: LPath[T] =
      Path2.asRelative(p)

    def foldMapA1[A: Semigroup](
      fileIn: FileName => A,
      dirIn: DirName => A,
      root: A
    ): A =
      Path2.foldMapA1(fileIn, dirIn, root, p)

    def foldMapA[A: Monoid](
      fileIn: FileName => A,
      dirIn: DirName => A
    ): A =
      Path2.foldMapA(fileIn, dirIn, p)
  }

  final implicit class RelOps[B <: Rel, T <: Typ](val p: Path2[B, T]) extends AnyVal {
    def maybeEsc: Option[Path2[Esc, T]] =
      Path2.maybeEsc(p)

    def maybeLoc: Option[Path2[Loc, T]] =
      Path2.maybeLoc(p)

    def refineRel: Path2[Esc, T] \/ Path2[Loc, T] =
      Path2.refineRel(p)
  }

  final implicit class LocOps[T <: Typ](val p: Path2[Loc, T]) extends AnyVal {
    def foldMapL1[A: Semigroup](
      fileIn: FileName => A,
      dirIn: DirName => A,
      cur: A
    ): A =
      Path2.foldMapL1(fileIn, dirIn, cur, p)

    def foldMapL[A: Monoid](
      fileIn: FileName => A,
      dirIn: DirName => A
    ): A =
      Path2.foldMapL(fileIn, dirIn, p)
  }

  final implicit class EscOps[T <: Typ](val p: Path2[Esc, T]) extends AnyVal {
    def sandbox[B <: Base : Sandboxed](d: Path2[B, Dir]): Option[Path2[B, T]] =
      Path2.sandbox(p, d)
  }

  //--- Instances ---

  implicit def pathEqual[B <: Base, T <: Typ]: Equal[Path2[B, T]] =
    Equal.equalA

  // TODO: Replace w/posix codec
  implicit def pathShow[B <: Base, T <: Typ]: Show[Path2[B, T]] =
    Show.shows(foldMap1(_.value, _.value + "/", "../", "./", "/", _))
}
