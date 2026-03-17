package it.unibo.pps.u03

import it.unibo.pps.u03.Persons.Person.{Student, Teacher, getCourses}
import org.junit.Assert.assertEquals
import org.junit.Test
import u03.Sequences.Sequence.{Cons, Nil, foldLeft}

class PersonTest:
  @Test def testGetCourses(): Unit =
    val persons = Cons(Teacher("Mirko Viroli", "PPS"), Cons(Student("Mario Rossi", 24), Cons(Teacher("Alessandro Ricci", "PCD"), Nil())))
    assertEquals(Cons("PPS", Cons("PCD", Nil())), getCourses(persons))

  @Test def testFoldLeftWithInt(): Unit =
    val s = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(s)(0)(_ - _))

  @Test def testFoldLeftWithString(): Unit =
    val s = Cons("a", Cons("b", Cons("c", Cons("d", Nil()))))
    assertEquals("abcd", foldLeft(s)("")(_ + _))