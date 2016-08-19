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

package object pathy {
  import Path2._

  type APath[T <: Typ] = Path2[Abs, T]
  type LPath[T <: Typ] = Path2[Loc, T]
  type EPath[T <: Typ] = Path2[Esc, T]
  type RPath[T <: Typ] = Path2[_ <: Rel, T]

  type AFile = APath[File]
  type LFile = LPath[File]
  type EFile = EPath[File]

  type ADir = APath[Dir]
  type LDir = LPath[Dir]
  type EDir = EPath[Dir]

  //--- Constructors ---

  def file(name: String): LFile =
    file1(FileName(name))

  def file1(name: FileName): LFile =
    FileIn(cur, name)

  def dir(name: String): LDir =
    dir1(DirName(name))

  def dir1(name: DirName): LDir =
    DirIn(cur, name)

  val cur: LDir = Current

  val root: ADir = Root

  //--- Combinators ---

  def append[B <: Base, T <: Typ](d: Path2[B, Dir], p: LPath[T]): Path2[B, T] =
    p match {
      case FileIn(pp, n) => FileIn(append(d, pp), n)
      case DirIn(pp, n)  => DirIn(append(d, pp), n)
      case Current       => d
    }

  def asRelative[T <: Typ]: APath[T] => LPath[T] = {
    case FileIn(p, n) => FileIn(asRelative(p), n)
    case DirIn(p, n)  => DirIn(asRelative(p), n)
    case Root         => cur
  }

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
    root: A,
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

  def foldMapA[A: Monoid](
    fileIn: FileName => A,
    dirIn: DirName => A,
    path: APath[_]
  ): A =
    foldMapA1(fileIn, dirIn, mzero[A], path)

  def foldMapL1[A: Semigroup](
    fileIn: FileName => A,
    dirIn: DirName => A,
    cur: A,
    path: LPath[_]
  ): A = {
    @tailrec
    def go(rp: LPath[_], a: A): A = rp match {
      case FileIn(p, n) => go(p, fileIn(n) |+| a)
      case DirIn(p, n)  => go(p, dirIn(n) |+| a)
      case Current      => cur |+| a
    }

    path match {
      case FileIn(p, n) => go(p, fileIn(n))
      case DirIn(p, n)  => go(p, dirIn(n))
      case Current      => cur
    }
  }

  def foldMapL[A: Monoid](
    fileIn: FileName => A,
    dirIn: DirName => A,
    path: LPath[_]
  ): A =
    foldMapL1(fileIn, dirIn, mzero[A], path)

  val isAbsolute: Path2[_, _] => Boolean =
    (foldMap1(
      const(Conjunction(true)),
      const(Conjunction(true)),
      Conjunction(false),
      Conjunction(false),
      Conjunction(true),
      _: Path2[_, _]
    )) andThen Conjunction.unwrap

  val isRelative: Path2[_, _] => Boolean =
    isAbsolute andThen (!_)

  def maybeDir[B <: Base]: Path2[B, _] => Option[Path2[B, Dir]] =
    refineType(_).swap.toOption

  def maybeEsc[T <: Typ]: RPath[T] => Option[Path2[Esc, T]] =
    refineRel(_).swap.toOption

  def maybeFile[B <: Base]: Path2[B, _] => Option[Path2[B, File]] =
    refineType(_).toOption

  def maybeLoc[T <: Typ]: RPath[T] => Option[Path2[Loc, T]] =
    refineRel(_).toOption

  def parentDir[B <: Base](p: Path2[B, _])(implicit B: ParentDir[B]): B.Out =
    B.parentDir(p)

  def peel[B <: Base]: Path2[B, _] => Option[(Path2[B, Dir], DirName \/ FileName)] = {
    case FileIn(d, n) => some((d, n.right))
    case DirIn(d, n)  => some((d, n.left))
    case ParentIn(d)  => none
    case Current      => none
    case Root         => none
  }

  def refineRel[B <: Rel, T <: Typ]: Path2[B, T] => Path2[Esc, T] \/ Path2[Loc, T] = {
    // NB: scalac doesn't seem to be able to preserve the more specific
    //     constraint that B <: Rel when pattern matching, even though FileIn / DirIn
    //     are parametric in B <: Base.
    case FileIn(d, n) =>
      refineRel(d.asInstanceOf[Path2[B, Dir]]).bimap(_ / file1(n), _ / file1(n))
    case DirIn(d, n)  =>
      refineRel(d.asInstanceOf[Path2[B, Dir]]).bimap(_ / dir1(n), _ / dir1(n))
    case ParentIn(d) =>
      ParentIn(d).left
    case Current =>
      cur.right
    case Root =>
      scala.sys.error("because, scalac.")
  }

  def refineType[B <: Base]: Path2[B, _] => Path2[B, Dir] \/ Path2[B, File] = {
    case FileIn(d, n) => FileIn(d, n).right
    case DirIn(d, n)  => DirIn(d, n).left
    case ParentIn(d)  => ParentIn(d).left
    case Current      => cur.left
    case Root         => root.left
  }

  def relativeTo[B <: Base, T <: Typ](p: Path2[B, T], d: Path2[B, Dir])(implicit R: RelativeTo[B]): Option[R.Out[T]] =
    R.relativeTo(p, d)

  def renameDir[B <: Base](f: DirName => DirName): Path2[B, Dir] => Path2[B, Dir] = {
    case DirIn(d, n)     => DirIn(d, f(n))
    case p @ ParentIn(_) => p
    case c @ Current     => c
    case r @ Root        => r
  }

  def renameFile[B <: Base](f: FileName => FileName): Path2[B, File] => Path2[B, File] = {
    case FileIn(d, n) => FileIn(d, f(n))
  }

  def sandbox[B <: Base, T <: Typ](
    e: Path2[Esc, T],
    p: Path2[B, Dir])(
    implicit B: Sandboxed[B]
  ): Option[Path2[B, T]] =
    B.sandbox(e, p)
}
