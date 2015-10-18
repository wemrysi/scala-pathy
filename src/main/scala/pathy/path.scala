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

import scala.annotation.tailrec
import scalaz._, Scalaz._, Leibniz._

sealed trait Path[B,T,S] {
  import Path._

  def foldT[X](
    root: (B === Abs, T === Dir) => X,
    current: (B === Rel, T === Dir) => X,
    parent: (Path[B,Dir,S], T === Dir) => X,
    dir: (Path[B,Dir,S], DirName, T === Dir) => X,
    file: (Path[B,Dir,S], FileName, T === File) => X
  ): X

  def fold[X](
    root: => X,
    current: => X,
    parent: (Path[B,Dir,S]) => X,
    dir: (Path[B,Dir,S], DirName) => X,
    file: (Path[B,Dir,S], FileName) => X
  ): X =
    foldT(
      (_, _)    => root,
      (_, _)    => current,
      (d, _)    => parent(d),
      (d, n, _) => dir(d, n),
      (d, n, _) => file(d, n))
}

object Path {
  sealed trait Rel
  sealed trait Abs

  sealed trait File
  sealed trait Dir

  sealed trait Sandboxed
  sealed trait Unsandboxed

  final case class FileName(value: String) extends AnyVal {
    def extension: String = {
      val idx = value.lastIndexOf(".")
      if (idx == -1) "" else value.substring(idx + 1)
    }

    def dropExtension: FileName = {
      val idx = value.lastIndexOf(".")
      if (idx == -1) this else FileName(value.substring(0, idx))
    }

    def changeExtension(f: String => String): FileName =
      FileName(dropExtension.value + "." + f(extension))
  }

  final case class DirName(value: String) extends AnyVal

  // Note: this ADT allows invalid paths, but the exposed functions
  // of the package do not.
  private final case class Root[S]() extends Path[Abs,Dir,S] {
    def foldT[X](
      root: (Abs === Abs, Dir === Dir) => X,
      current: (Abs === Rel, Dir === Dir) => X,
      parent: (Path[Abs,Dir,S], Dir === Dir) => X,
      dir: (Path[Abs,Dir,S], DirName, Dir === Dir) => X,
      file: (Path[Abs,Dir,S], FileName, Dir === File) => X
    ): X = root(refl[Abs], refl[Dir])
  }
  private final case class Current[S]() extends Path[Rel,Dir,S] {
    def foldT[X](
      root: (Rel === Abs, Dir === Dir) => X,
      current: (Rel === Rel, Dir === Dir) => X,
      parent: (Path[Rel,Dir,S], Dir === Dir) => X,
      dir: (Path[Rel,Dir,S], DirName, Dir === Dir) => X,
      file: (Path[Rel,Dir,S], FileName, Dir === File) => X
    ): X = current(refl[Rel], refl[Dir])
  }
  private final case class ParentIn[B,S](parentDir: Path[B,Dir,S]) extends Path[B,Dir,S] {
    def foldT[X](
      root: (B === Abs, Dir === Dir) => X,
      current: (B === Rel, Dir === Dir) => X,
      parent: (Path[B,Dir,S], Dir === Dir) => X,
      dir: (Path[B,Dir,S], DirName, Dir === Dir) => X,
      file: (Path[B,Dir,S], FileName, Dir === File) => X
    ): X = parent(parentDir, refl[Dir])
  }
  private final case class DirIn[B,S](parentDir: Path[B,Dir,S], name: DirName) extends Path[B,Dir,S] {
    def foldT[X](
      root: (B === Abs, Dir === Dir) => X,
      current: (B === Rel, Dir === Dir) => X,
      parent: (Path[B,Dir,S], Dir === Dir) => X,
      dir: (Path[B,Dir,S], DirName, Dir === Dir) => X,
      file: (Path[B,Dir,S], FileName, Dir === File) => X
    ): X = dir(parentDir, name, refl[Dir])
  }
  private final case class FileIn[B,S](parentDir: Path[B,Dir,S], name: FileName) extends Path[B,File,S] {
    def foldT[X](
      root: (B === Abs, File === Dir) => X,
      current: (B === Rel, File === Dir) => X,
      parent: (Path[B,Dir,S], File === Dir) => X,
      dir: (Path[B,Dir,S], DirName, File === Dir) => X,
      file: (Path[B,Dir,S], FileName, File === File) => X
    ): X = file(parentDir, name, refl[File])
  }

  private def parentIn[B,S](p: Path[B,Dir,S]): Path[B,Dir,S] =
    ParentIn(p)

  private def dirIn[B,S](p: Path[B,Dir,S], n: DirName): Path[B,Dir,S] =
    DirIn(p, n)

  private def fileIn[B,S](p: Path[B,Dir,S], n: FileName): Path[B,File,S] =
    FileIn(p, n)

  private final implicit class LeibnizOps[X, Y](e: X === Y) {
    def flip: Y === X = symm[Nothing, Any, X, Y](e)

    def B[T,S]: Path[X,T,S] => Path[Y,T,S] =
      e.subst[({type f[a] = Path[a,T,S]})#f]

    def T[B,S]: Path[B,X,S] => Path[B,Y,S] =
      e.subst[({type f[a] = Path[B,a,S]})#f]

    def >&>[U,V,S](o: U === V): Path[X,U,S] => Path[Y,V,S] =
      o.T[Y,S] compose B[U,S]

    def <&<[U,V,S](o: U === V): Path[Y,V,S] => Path[X,U,S] =
      o.flip.T[X,S] compose flip.B[V,S]
  }

  private def impossible[A]: A = sys.error("impossible!")

  type RelFile[S] = Path[Rel,File,S]
  type AbsFile[S] = Path[Abs,File,S]
  type RelDir[S] = Path[Rel,Dir,S]
  type AbsDir[S] = Path[Abs,Dir,S]

  def rootDir[S]: Path[Abs,Dir,S] = Root()

  def currentDir[S]: Path[Rel,Dir,S] = Current()

  def dir[S](name: String): Path[Rel,Dir,S] = dir1(DirName(name))

  def dir1[S](name: DirName): Path[Rel,Dir,S] = dirIn(currentDir, name)

  def dirName[B,S](path: Path[B,Dir,S]): Option[DirName] =
    path.fold(None, None, _ => None, (_, n) => Some(n), (_, _) => None)

  def file[S](name: String): Path[Rel,File,S] = file1(FileName(name))

  def file1[S](name: FileName): Path[Rel,File,S] = fileIn(currentDir, name)

  def fileName[B,S](path: Path[B,File,S]): FileName =
    path.foldT(
      (_, _)    => impossible,
      (_, _)    => impossible,
      (_, _)    => impossible,
      (_, _, _) => impossible,
      (_, n, _) => n)

  implicit class PathOps[B,T,S](path: Path[B,T,S]) {
    def relativeTo[SS](dir: Path[B,Dir,SS]): Option[Path[Rel,T,SS]] = {
      def go[TT](p1: Path[B,TT,S], p2: Path[B,Dir,SS]): Option[Path[Rel,TT,SS]] =
        peelT(p1) match {
          case Some((p1p, -\/((d, t)))) =>
            go(p1p, p2) map (_ </> t.flip.T(dir1(d)))

          case Some((p1p, \/-((f, t)))) =>
            go(p1p, p2) map (_ </> t.flip.T(file1(f)))

          case None =>
            p1.foldT(
              (_, t) => p2.fold(
                t.flip.T(currentDir[SS]).some,
                None, _ => None, (_, _) => None, (_, _) => None),

              (_, t) => p2.fold(
                None,
                t.flip.T(currentDir[SS]).some,
                _ => None, (_, _) => None, (_, _) => None),

              (_, _) => None, (_, _, _) => None, (_, _, _) => None)
        }

      go(canonicalize(path), canonicalize(dir))
    }
  }

  implicit class DirOps[B,S](dir: Path[B,Dir,S]) {
    def </>[T](rel: Path[Rel,T,S]): Path[B,T,S] =
      rel.foldT[Path[B,T,S]](
        (_, _)    => impossible,
        (_, t0)   => t0.flip.T(dir.foldT(
                       (b, t)    => (b <&< t)(rootDir),
                       (b, t)    => (b <&< t)(currentDir),
                       (d, t)    => t.flip.T(parentIn(d </> currentDir)),
                       (d, n, t) => t.flip.T(dirIn(d </> currentDir, n)),
                       (_, _, _) => impossible)),
        (d, t)    => t.flip.T(parentIn(dir </> d)),
        (d, n, t) => t.flip.T(dirIn(dir </> d, n)),
        (d, n, t) => t.flip.T(fileIn(dir </> d, n)))

    // NB: scala doesn't cotton to `<..>`
    // TODO: Should this return Option[Path[B,T,Unsandboxed]] and fail if dir == Root?
    def <::>[T](rel: Path[Rel,T,S]): Path[B,T,Unsandboxed] =
      parentDir1(dir) </> unsandbox(rel)
  }

  implicit class FileOps[B,S](file: Path[B,File,S]) {
    // NB: scala doesn't cotton to `<.>`
    def <:>(ext: String): Path[B,File,S] =
      renameFile(file, name => name.changeExtension(_ => ext))
  }

  def refineType[B,T,S](path: Path[B,T,S]): Path[B,Dir,S] \/ Path[B,File,S] =
    path.foldT(
      (_, t)    => t.T(path).left,
      (_, t)    => t.T(path).left,
      (_, t)    => t.T(path).left,
      (_, _, t) => t.T(path).left,
      (_, _, t) => t.T(path).right)

  def maybeDir[B,T,S](path: Path[B,T,S]): Option[Path[B, Dir, S]] =
    refineType(path).swap.toOption

  def maybeFile[B,T,S](path: Path[B,T,S]): Option[Path[B, File, S]] =
    refineType(path).toOption

  def peelT[B,T,S](path: Path[B,T,S]): Option[(Path[B,Dir,S], (DirName, T === Dir) \/ (FileName, T === File))] =
    path.foldT(
      (_, _)    => None,
      (_, _)    => None,
      (_, _)    => {
        val (chg, p1) = canonicalize1(path)
        if (chg) peelT(p1) else None
      },
      (d, n, t) => Some((d, (n, t).left)),
      (d, n, t) => Some((d, (n, t).right)))

  def peel[B,T,S](path: Path[B,T,S]): Option[(Path[B,Dir,S], DirName \/ FileName)] =
    peelT(path) map (_ map (_ bimap (_._1, _._1)))

  def depth(path: Path[_,_,_]): Int = {
    @tailrec
    def depth0(cur: Int, p: Path[_,_,_]): Int = {
      val r = p.fold(
        cur.left,
        cur.left,
        d      => (-1, d).right,
        (d, _) => (1, d).right,
        (d, _) => (1, d).right)

      r match {
        case -\/(n)      => n
        case \/-((n, d)) => depth0(cur + n, d)
      }
    }

    depth0(0, path)
  }

  def parentDir[B,T,S](path: Path[B,T,S]): Option[Path[B,Dir,S]] =
    peel(path).map(_._1)

  def fileParent[B,S](file: Path[B,File,S]): Path[B,Dir,S] =
    file.foldT(
      (_, _)    => impossible,
      (_, _)    => impossible,
      (_, _)    => impossible,
      (_, _, _) => impossible,
      (d, _, _) => d)

  def unsandbox[B,T,S](path: Path[B,T,S]): Path[B,T,Unsandboxed] =
    path.asInstanceOf[Path[B,T,Unsandboxed]]

  /** Synonym for relativeTo, constrained to sandboxed dirs, and with a more evocative name. */
  def sandbox[B,T,S](dir: Path[B,Dir,Sandboxed], path: Path[B,T,S]): Option[Path[Rel,T,Sandboxed]] =
    path relativeTo dir

  def parentDir1[B,S](path: Path[B,Dir,S]): Path[B,Dir,Unsandboxed] =
    parentIn(unsandbox(path))

  def renameFile[B,S](path: Path[B,File,S], f: FileName => FileName): Path[B,File,S] =
    path.foldT(
      (_, _)    => impossible,
      (_, _)    => impossible,
      (_, _)    => impossible,
      (_, _, _) => impossible,
      (d, n, _) => fileIn(d, f(n)))

  def renameDir[B,S](path: Path[B,Dir,S], f: DirName => DirName): Path[B,Dir,S] =
    path.foldT(
      (_, _)    => path,
      (_, _)    => path,
      (_, _)    => path,
      (d, n, _) => dirIn(d, f(n)),
      (_, _, _) => impossible)

  def canonicalize[B,T,S](path: Path[B,T,S]): Path[B,T,S] =
    canonicalize1(path)._2

  private def canonicalize1[B,T,S](path: Path[B,T,S]): (Boolean, Path[B,T,S]) = {
    def canonicalizeParent(d: Path[B,Dir,S]) = {
      val (chg, d1) = canonicalize1(d)
      val d2        = parentIn(d1)
      if (chg) canonicalize1(d2) else (chg, d2)
    }

    path.foldT(
      (_, _)    => (false, path),
      (_, _)    => (false, path),
      (d, t)    => d.foldT(
        (_, _)    => canonicalizeParent(d) map (t.flip.T),
        (_, _)    => canonicalizeParent(d) map (t.flip.T),
        (_, _)    => canonicalizeParent(d) map (t.flip.T),
        (d, _, _) => (true, t.flip.T(canonicalize1(d)._2)),
        (_, _, _) => impossible),
      (d, n, t) => {
        val (chg, d1) = canonicalize1(d)
        (chg, t.flip.T(dirIn(d1, n)))
      },
      (d, n, t) => {
        val (chg, d1) = canonicalize1(d)
        (chg, t.flip.T(fileIn(d1, n)))
      })
  }

  def flatten[X](root: => X, currentDir: => X, parentDir: => X, dirName: String => X, fileName: String => X, path: Path[_, _, _]): OneAnd[IList, X] = {
    @tailrec
    def go(xs: OneAnd[IList, X], at: Path[_, _, _]): OneAnd[IList, X] = {
      val tl = xs.head :: xs.tail
      val r = at.fold(
        OneAnd(root, tl).left,
        OneAnd(currentDir, tl).left,
        p      => (OneAnd(parentDir, tl), p).right,
        (p, d) => (OneAnd(dirName(d.value), tl), p).right,
        (p, f) => (OneAnd(fileName(f.value), tl), p).right)

      r match {
        case -\/(xs)        => xs
        case \/-((xs, nxt)) => go(xs, nxt)
      }
    }

    path.fold(
      OneAnd(root, IList.empty),
      OneAnd(currentDir, IList.empty),
      p      => go(OneAnd(parentDir, IList.empty), p),
      (p, d) => go(OneAnd(dirName(d.value), IList.empty), p),
      (p, f) => go(OneAnd(fileName(f.value), IList.empty), p))
  }

  def identicalPath[B,T,S,BB,TT,SS](p1: Path[B,T,S], p2: Path[BB,TT,SS]): Boolean =
    p1.shows == p2.shows
/*
  val posixCodec = PathCodec placeholder '/'

  val windowsCodec = PathCodec placeholder '\\'

  final case class PathCodec(separator: Char, escape: String => String, unescape: String => String) {

    def unsafePrintPath(path: Path[_, _, _]): String = {
      val s = flatten("", ".", "..", escape, escape, path)
                .intercalate(separator.toString)

      maybeDir(path) ? (s + separator) | s
    }

    def printPath[B T](path: Path[B,T,Sandboxed]): String =
      unsafePrintPath(path)

    def parsePath[Z](
      rf: RelFile[Unsandboxed] => Z,
      af: AbsFile[Unsandboxed] => Z,
      rd: RelDir[Unsandboxed] => Z,
      ad: AbsDir[Unsandboxed] => Z)(str: String): Z =
    {
      val segs = str.split(separator)
      val last = segs.length - 1
      val isAbs = str.startsWith(separator.toString)
      val isFile = !str.endsWith(separator.toString)
      val tuples = segs.zipWithIndex

      def folder[B,T,S](base: Path[B,T,S], t: (String, Int)): Path[B,T,S] = t match {
        case ("", _)    => base
        case (".", _)   => base
        case ("..", _)  => ParentIn(base)
        case (seg, idx) =>
          if (isFile && idx == last)
            FileIn(base, FileName(unescape(seg)))
          else
            DirIn(base, DirName(unescape(seg)))
      }

      if (str == "")
        rd(Current)
      else if (isAbs && isFile)
        af(tuples.foldLeft[AbsFile[Unsandboxed]](Root)(folder))
      else if (isAbs && !isFile)
        ad(tuples.foldLeft[AbsDir[Unsandboxed]](Root)(folder))
      else if (!isAbs && isFile)
        rf(tuples.foldLeft[RelFile[Unsandboxed]](Current)(folder))
      else
        rd(tuples.foldLeft[RelDir[Unsandboxed]](Current)(folder))
    }

    val parseRelFile: String => Option[RelFile[Unsandboxed]] =
      parsePath[Option[RelFile[Unsandboxed]]](Some(_), _ => None, _ => None, _ => None)

    val parseAbsFile: String => Option[AbsFile[Unsandboxed]] =
      parsePath[Option[AbsFile[Unsandboxed]]](_ => None, Some(_), _ => None, _ => None)

    val parseRelDir: String => Option[RelDir[Unsandboxed]] =
      parsePath[Option[RelDir[Unsandboxed]]](_ => None, _ => None, Some(_), _ => None)

    val parseAbsDir: String => Option[AbsDir[Unsandboxed]] =
      parsePath[Option[AbsDir[Unsandboxed]]](_ => None, _ => None, _ => None, Some(_))

    private def asDir[B,S](path: Path[B, File, S]): Path[B, Dir, S] = path match {
      case FileIn(p, FileName(n)) => DirIn(unsafeCoerceType(p), DirName(n))
      case _ => sys.error("impossible!")
    }

    val parseRelAsDir: String => Option[RelDir[Unsandboxed]] =
      parsePath[Option[RelDir[Unsandboxed]]](p => Some(asDir(p)), _ => None, Some(_), _ => None)

    val parseAbsAsDir: String => Option[AbsDir[Unsandboxed]] =
      parsePath[Option[AbsDir[Unsandboxed]]](_ => None, p => Some(asDir(p)), _ => None, Some(_))

  }

  object PathCodec {

    /**
     * The placeholder codec, replaces literal instances of the separator
     * in segments with a placeholder as well as segments equal to either of the
     * relative dir literals, "." and "..".
     */
    def placeholder(sep: Char): PathCodec = {
      val escapeSep = (_: String).replaceAllLiterally(sep.toString, $sep$)
      val unescapeSep = (_: String).replaceAllLiterally($sep$, sep.toString)

      PathCodec(sep, escapeRel compose escapeSep, unescapeSep compose unescapeRel)
    }

    private val escapeRel = (s: String) =>
      if (s == "..") $dotdot$ else if (s == ".") $dot$ else s

    private val unescapeRel = (s: String) =>
      if (s == $dotdot$) ".." else if (s == $dot$) "." else s

    private val $sep$ = "$sep$"
    private val $dot$ = "$dot$"
    private val $dotdot$ = "$dotdot$"
  }
*/
  implicit def PathShow[B,T,S]: Show[Path[B,T,S]] = new Show[Path[B,T,S]] {
    override def show(v: Path[B,T,S]) =
      v.fold("rootDir", "currentDir",
        p      => "parentDir(" + p.show + ")",
        (p, d) => p.show + " </> dir(" + d.value.show + ")",
        (p, f) => p.show + " </> file(" + f.value.show + ")")
  }
}
