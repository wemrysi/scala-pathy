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
import scalaz._
import scalaz.std.anyVal._
import scalaz.std.option._
import scalaz.syntax.either._
import scalaz.syntax.monoid._

/** Goals
  * 1. more type safety, make more invalid states unrepresentable
  * 2. nicer impl, take advantage of GADTs to avoid casting
  * 3. increaased correctness
  *   only allow parentIn to appear at the beginning of a path, i.e.
  *   paths are always "canonical"/"normalized".
  * 4. nicer syntax, would like / "foo" / "bar" / "baz" <= dir, what about file??
  */

sealed abstract class Path2[B <: Path2.Base, T <: Path2.Typ]

object Path2 {
  sealed trait Base
  sealed trait Abs extends Base
  sealed trait Rel extends Base

  sealed trait Typ
  sealed trait File extends Typ
  sealed trait Dir extends Typ

  type RPath[T <: Typ] = Path2[Rel, T]
  type APath[T <: Typ] = Path2[Abs, T]

  type RFile = RPath[File]
  type AFile = APath[File]

  type RDir = RPath[Dir]
  type ADir = APath[Dir]

  final case class FileName(value: String) extends AnyVal {
    def dropExtension: FileName = {
      val idx = value.lastIndexOf(".")
      if (idx == -1) this else FileName(value.substring(0, idx))
    }

    def extension: String = {
      val idx = value.lastIndexOf(".")
      if (idx == -1) "" else value.substring(idx + 1)
    }

    def modifyExtension(f: String => String): FileName =
      FileName(dropExtension.value + "." + f(extension))
  }

  final case class DirName(value: String) extends AnyVal

  private final case object Root
    extends Path2[Abs, Dir]
  private final case object Current
    extends Path2[Rel, Dir]
  private final case class ParentIn(parent: Path2[Rel, Dir])
    extends Path2[Rel, Dir]
  private final case class DirIn[B <: Base](parent: Path2[B, Dir], name: DirName)
    extends Path2[B, Dir]
  private final case class FileIn[B <: Base](parent: Path2[B, Dir], name: FileName)
    extends Path2[B, File]

  //--- Constructors ---

  val rootDir: Path2[Abs, Dir] = Root
  val / : Path2[Abs, Dir] = rootDir

  val currentDir: Path2[Rel, Dir] = Current
  // Eh.
  val `./` : Path2[Rel, Dir] = currentDir
  val `../` : Path2[Rel, Dir] = ParentIn(currentDir)

  def dir(name: String): RDir =
    dir1(DirName(name))

  def dir1(name: DirName): RDir =
    DirIn(currentDir, name)

  def file(name: String): RFile =
    file1(FileName(name))

  def file1(name: FileName): RFile =
    FileIn(currentDir, name)

  //--- Combinators ---

  val depth: Path2[_, _] => Int =
    foldMap(const(1), const(1), -1, _)

  val dirName: Path2[_, Dir] => Option[DirName] = {
    case DirIn(_, n) => some(n)
    case ParentIn(_) => none
    case Current     => none
    case Root        => none
  }

  val fileName: Path2[_, File] => FileName = {
    case FileIn(_, n) => n
  }

  def fileParent[B <: Base]: Path2[B, File] => Path2[B, Dir] = {
    case FileIn(p, _) => p
  }

  def foldMap1[A: Semigroup](
    fileIn: FileName => A,
    dirIn: DirName => A,
    parentIn: => A,
    cur: => A,
    root: => A,
    path: Path2[_, _]
  ): A = {
    @tailrec
    def go(pth: Path2[_, _], a: A): A = pth match {
      case FileIn(p, n) => go(p, fileIn(n) |+| a)
      case DirIn(p, n)  => go(p, dirIn(n) |+| a)
      case ParentIn(p)  => go(p, parentIn |+| a)
      case Current      => cur |+| a
      case Root         => root |+| a
    }

    path match {
      case FileIn(p, n) => go(p, fileIn(n))
      case DirIn(p, n)  => go(p, dirIn(n))
      case ParentIn(p)  => go(p, parentIn)
      case Current      => cur
      case Root         => root
    }
  }

  def foldMap[A: Monoid](
    fileIn: FileName => A,
    dirIn: DirName => A,
    parentIn: => A,
    path: Path2[_, _]
  ): A =
    foldMap1(fileIn, dirIn, parentIn, mzero[A], mzero[A], path)

  def foldMapA1[A: Semigroup](
    fileIn: FileName => A,
    dirIn: DirName => A,
    root: => A,
    path: APath[_]
  ): A = {
    @tailrec
    def go(ap: APath[_], a: A): A = ap match {
      case FileIn(p, n) => go(p, fileIn(n) |+| a)
      case DirIn(p, n)  => go(p, dirIn(n) |+| a)
      case Root         => root |+| a
    }

    path match {
      case FileIn(p, n) => go(p, fileIn(n))
      case DirIn(p, n)  => go(p, dirIn(n))
      case Root         => root
    }
  }

  def foldMapA[A: Monoid](fileIn: FileName => A, dirIn: DirName => A, path: APath[_]): A =
    foldMapA1(fileIn, dirIn, mzero[A], path)

  def foldMapR1[A: Semigroup](
    fileIn: FileName => A,
    dirIn: DirName => A,
    parentIn: => A,
    cur: => A,
    path: RPath[_]
  ): A = {
    @tailrec
    def go(rp: RPath[_], a: A): A = rp match {
      case FileIn(p, n) => go(p, fileIn(n) |+| a)
      case DirIn(p, n)  => go(p, dirIn(n) |+| a)
      case ParentIn(p)  => go(p, parentIn |+| a)
      case Current      => cur |+| a
    }

    path match {
      case FileIn(p, n) => go(p, fileIn(n))
      case DirIn(p, n)  => go(p, dirIn(n))
      case ParentIn(p)  => go(p, parentIn)
      case Current      => cur
    }
  }

  def foldMapR[A: Monoid](
    fileIn: FileName => A,
    dirIn: DirName => A,
    parentIn: => A,
    path: RPath[_]
  ): A =
    foldMapR1(fileIn, dirIn, parentIn, mzero[A], path)

  def maybeDir[B <: Base]: Path2[B, _] => Option[Path2[B, Dir]] =
    refineType(_).swap.toOption

  def maybeFile[B <: Base]: Path2[B, _] => Option[Path2[B, File]] =
    refineType(_).toOption

  def parentDir[B <: Base]: Path2[B, _] => Option[Path2[B, Dir]] =
    peel(_).map(_._1)

  def peel[B <: Base]: Path2[B, _] => Option[(Path2[B, Dir], DirName \/ FileName)] = {
    case FileIn(d, n) => some((d, n.right))
    case DirIn(d, n)  => some((d, n.left))
    case ParentIn(d)  => none
    case Current      => none
    case Root         => none
  }

  def refineType[B <: Base]: Path2[B, _] => Path2[B, Dir] \/ Path2[B, File] = {
    case FileIn(d, n) => FileIn(d, n).right
    case DirIn(d, n)  => DirIn(d, n).left
    case ParentIn(d)  => ParentIn(d).left
    case Current      => currentDir.left
    case Root         => rootDir.left
  }

  def renameDir[B <: Base](f: DirName => DirName): Path2[B, Dir] => Path2[B, Dir] = {
    case DirIn(d, n)     => DirIn(d, f(n))
    case p @ ParentIn(_) => p
    case c @ Current     => c
    case r @ Root        => r
  }

  def renameFile[B <: Base](f: FileName => FileName): Path2[B, File] => Path2[B, File] = {
    case FileIn(d, n) => FileIn(d, f(n))
  }
}
