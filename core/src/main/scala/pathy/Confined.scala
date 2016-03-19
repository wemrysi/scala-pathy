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

/** Typelevel predicate indicating that the given `Base` does not escape
  * its context.
  *
  * TODO: Better name for what this is trying to convey.
  */
sealed abstract class Confined[B <: Path2.Base]

object Confined {
  import Path2.{Abs, Loc}

  implicit val absIsConfined: Confined[Abs] =
    new Confined[Abs] {}

  implicit val locIsConfined: Confined[Loc] =
    new Confined[Loc] {}
}
