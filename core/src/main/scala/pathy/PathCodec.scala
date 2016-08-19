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

import scalaz._

final class PathCodec2(
  separator: Char,
  escape: String => String,
  unescape: String => String
) {

  def printPath(path: Path2[_, _]): String = {
    val s = path.foldMap1(
      fn => DList(escape(fn.value)),
      dn => DList(escape(dn.value)),
      DList(".."),
      DList("."),
      DList()
    ).intercalate(separator.toString)

    path.maybeDir ? (s + separator) | s
  }

  def parsePath[Z](
    ef: EFile => Z,
    lf: LFile => Z,
    af: AFile => Z,
    ed: EDir  => Z,
    ld: LDir  => Z,
    ad: ADir  => Z
  )(
    str: String
  ): Z = {
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

object PathCodec2 {

  /**
   * The placeholder codec, replaces literal instances of the separator
   * in segments with a placeholder as well as segments equal to either of the
   * relative dir literals, "." and "..".
   */
  def placeholder(sep: Char): PathCodec2 = {
    val escapeSep = (_: String).replaceAllLiterally(sep.toString, $sep$)
    val unescapeSep = (_: String).replaceAllLiterally($sep$, sep.toString)

    PathCodec2(sep, escapeRel compose escapeSep, unescapeSep compose unescapeRel)
  }

  val posixCodec = PathCodec2 placeholder '/'

  val windowsCodec = PathCodec2 placeholder '\\'

  private val escapeRel = (s: String) =>
    if (s == "..") $dotdot$ else if (s == ".") $dot$ else s

  private val unescapeRel = (s: String) =>
    if (s == $dotdot$) ".." else if (s == $dot$) "." else s

  private val $sep$ = "$sep$"
  private val $dot$ = "$dot$"
  private val $dotdot$ = "$dotdot$"
}
