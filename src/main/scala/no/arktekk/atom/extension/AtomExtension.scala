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
import com.codecommit.antixml.QName


/**
 * @author Erlend Hamnaberg<erlend@hamnaberg.net>
 * @author Trygve Laugst&oslash;l<trygvis@inamo.no>
 */
trait AtomExtension[Like, A] {
  def fromLike(like: Like): A

  def toChildren(a: A): IndexedSeq[ElementWrapper]

  def toAttributes(a: A): Seq[(QName, String)] = Nil

  lazy val asF: (Like => (Like, A)) = like => (like, fromLike(like))

  def ++[B](b: AtomExtension[Like, B]): AtomExtension[Like, (A, B)] = {
    val self = this
    new AtomExtension[Like, (A, B)] {
      def fromLike(like: Like) = (self.fromLike(like), b.fromLike(like))

      def toChildren(t: (A, B)) = self.toChildren(t._1) ++ b.toChildren(t._2)

      override def toAttributes(t: (A, B)) = self.toAttributes(t._1) ++ b.toAttributes(t._2)
    }
  }
}

object AtomExtension {
  def apply[Like, A, B](a: AtomExtension[Like, A], b: AtomExtension[Like, B]): AtomExtension[Like, (A, B)] = new AtomExtension[Like, (A, B)] {
    def fromLike(like: Like) = (a.fromLike(like), b.fromLike(like))

    def toChildren(t: (A, B)): IndexedSeq[ElementWrapper] = a.toChildren(t._1) ++ b.toChildren(t._2)

    override def toAttributes(t: (A, B)) = a.toAttributes(t._1) ++ b.toAttributes(t._2)
  }

  def apply[Like, A, B, C](a: AtomExtension[Like, A], b: AtomExtension[Like, B], c: AtomExtension[Like, C]): AtomExtension[Like, (A, B, C)] = new AtomExtension[Like, (A, B, C)] {
    def fromLike(like: Like) = (a.fromLike(like), b.fromLike(like), c.fromLike(like))
    
    def toChildren(t: (A, B, C)): IndexedSeq[ElementWrapper] = a.toChildren(t._1) ++ b.toChildren(t._2) ++ c.toChildren(t._3)
    
    override def toAttributes(t: (A, B, C)) = a.toAttributes(t._1) ++ b.toAttributes(t._2) ++ c.toAttributes(t._3)
  }

  def apply[Like, A, B, C, D](a: AtomExtension[Like, A], b: AtomExtension[Like, B], c: AtomExtension[Like, C], d: AtomExtension[Like, D]): AtomExtension[Like, (A, B, C, D)] = new AtomExtension[Like, (A, B, C, D)] {
    def fromLike(like: Like) = (a.fromLike(like), b.fromLike(like), c.fromLike(like), d.fromLike(like))

    def toChildren(t: (A, B, C, D)): IndexedSeq[ElementWrapper] = a.toChildren(t._1) ++ b.toChildren(t._2) ++ c.toChildren(t._3) ++ d.toChildren (t._4)
    
    override def toAttributes(t: (A, B, C, D)) = a.toAttributes(t._1) ++ b.toAttributes(t._2) ++ c.toAttributes(t._3) ++ d.toAttributes(t._4)
  }

  def apply[Like, A, B, C, D, E](a: AtomExtension[Like, A], b: AtomExtension[Like, B], c: AtomExtension[Like, C], d: AtomExtension[Like, D], e: AtomExtension[Like, E]): AtomExtension[Like, (A, B, C, D, E)] = new AtomExtension[Like, (A, B, C, D, E)] {
    def fromLike(like: Like) = (a.fromLike(like), b.fromLike(like), c.fromLike(like), d.fromLike(like), e.fromLike(like))

    def toChildren(t: (A, B, C, D, E)): IndexedSeq[ElementWrapper] = a.toChildren(t._1) ++ b.toChildren(t._2) ++ c.toChildren(t._3) ++ d.toChildren(t._4) ++ e.toChildren(t._5)

    override def toAttributes(t: (A, B, C, D, E)) = a.toAttributes(t._1) ++ b.toAttributes(t._2) ++ c.toAttributes(t._3) ++ d.toAttributes(t._4) ++ e.toAttributes(t._5)
  }
}

