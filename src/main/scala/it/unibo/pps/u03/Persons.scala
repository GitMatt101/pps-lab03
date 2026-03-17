package it.unibo.pps.u03

import u03.Sequences.Sequence
import u03.Sequences.Sequence.{Cons, Nil, flatMap}

object Persons:
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:
    def name(p: Person): String = p match
      case Student(n, _) => n
      case Teacher(n, _) => n

    def getCourses(p: Sequence[Person]): Sequence[String] = flatMap(p)(person => person match
      case Teacher(_, c) => Cons(c, Nil())
      case _ => Nil()
    )
