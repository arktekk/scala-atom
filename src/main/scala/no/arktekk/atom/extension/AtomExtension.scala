/*
 * Copyright 2011 Arktekk AS
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
package no.arktekk.atom.extension

import no.arktekk.atom.ElementWrapper


/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 * @author Trygve Laugst&oslash;l<trygvis@inamo.no>
 */
trait AtomExtension[Like, A] {
  def fromLike(like: Like): A

  def toElem(a: A, wrapper: ElementWrapper): Seq[ElementWrapper]

  lazy val asF: (Like => (Like, A)) = like => (like, fromLike(like))

  def ++[B](b: AtomExtension[Like, B]): AtomExtension[Like, (A, B)] = new AtomExtension[Like, (A, B)] {
    def fromLike(like: Like) = (AtomExtension.this.fromLike(like), b.fromLike(like))

    def toElem(t: (A, B), wrapper: ElementWrapper): Seq[ElementWrapper] = AtomExtension.this.toElem(t._1, wrapper) ++ b.toElem(t._2, wrapper)
  }
}

object AtomExtension {
  def apply[Like <: ElementWrapper, A, B](a: AtomExtension[Like, A], b: AtomExtension[Like, B]): AtomExtension[Like, (A, B)] = new AtomExtension[Like, (A, B)] {
    def fromLike(like: Like) = (a.fromLike(like), b.fromLike(like))

    def toElem(t: (A, B), wrapper: ElementWrapper): Seq[ElementWrapper] = a.toElem(t._1, wrapper) ++ b.toElem(t._2, wrapper)
  }

  def apply[Like <: ElementWrapper, A, B, C](a: AtomExtension[Like, A], b: AtomExtension[Like, B], c: AtomExtension[Like, C]): AtomExtension[Like, (A, B, C)] = new AtomExtension[Like, (A, B, C)] {
    def fromLike(like: Like) = (a.fromLike(like), b.fromLike(like), c.fromLike(like))

    def toElem(t: (A, B, C), wrapper: ElementWrapper): Seq[ElementWrapper] = a.toElem(t._1, wrapper) ++ b.toElem(t._2, wrapper) ++ c.toElem(t._3, wrapper)
  }

  def apply[Like <: ElementWrapper, A, B, C, D](a: AtomExtension[Like, A], b: AtomExtension[Like, B], c: AtomExtension[Like, C], d: AtomExtension[Like, D]): AtomExtension[Like, (A, B, C, D)] = new AtomExtension[Like, (A, B, C, D)] {
    def fromLike(like: Like) = (a.fromLike(like), b.fromLike(like), c.fromLike(like), d.fromLike(like))

    def toElem(t: (A, B, C, D), wrapper: ElementWrapper): Seq[ElementWrapper] = a.toElem(t._1, wrapper) ++ b.toElem(t._2, wrapper) ++ c.toElem(t._3, wrapper) ++ d.toElem (t._4, wrapper)
  }

  def apply[Like <: ElementWrapper, A, B, C, D, E](a: AtomExtension[Like, A], b: AtomExtension[Like, B], c: AtomExtension[Like, C], d: AtomExtension[Like, D], e: AtomExtension[Like, E]): AtomExtension[Like, (A, B, C, D, E)] = new AtomExtension[Like, (A, B, C, D, E)] {
    def fromLike(like: Like) = (a.fromLike(like), b.fromLike(like), c.fromLike(like), d.fromLike(like), e.fromLike(like))

    def toElem(t: (A, B, C, D, E), wrapper: ElementWrapper): Seq[ElementWrapper] = a.toElem(t._1, wrapper) ++ b.toElem(t._2, wrapper) ++ c.toElem(t._3, wrapper) ++ d.toElem(t._4, wrapper) ++ e.toElem(t._5, wrapper)
  }
}
