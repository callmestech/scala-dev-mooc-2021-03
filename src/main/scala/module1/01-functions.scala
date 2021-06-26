package module1

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object functions {


  /**
   * Функции
   */



  // SAM Single Abstract Method


  // trait Function1[Int, Int]{ def apply(x: Int): Int}



  /**
   *  Задание 1. Написать ф-цию метод isEven, которая будет вычислять является ли число четным
   */
  def isEven(i: Int): Boolean = i % 2 == 0


  /**
   * Задание 2. Написать ф-цию метод isOdd, которая будет вычислять является ли число нечетным
   */
  def isOdd(i: Int): Boolean = !isEven(i)

  /**
   * Задание 3. Написать ф-цию метод filterEven, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются четными
   */
  def filterEven1(xs: Array[Int]): Array[Int] = xs.filter(isEven)

  def filterEven2(xs: Array[Int]): Array[Int] = filter(xs)(isEven)

  /**
   * Задание 3. Написать ф-цию метод filterOdd, которая получает на вход массив чисел и возвращает массив тех из них,
   * которые являются нечетными
   */
  def filterOdd1(xs: Array[Int]): Array[Int] = xs.filter(isOdd)

  def filterOdd2(xs: Array[Int]): Array[Int] = filter(xs)(isOdd)

  /**
   * Without use built-in functions
   * */
  def filter[A](xs: Array[A])(p: A => Boolean): Array[A] = {
    val buf: mutable.ArrayBuffer[A] = ArrayBuffer()

    for (i <- xs.indices) {
      if (p(xs(i))) {
        buf :+ xs(i)
      }
    }

    buf.toArray
  }
  /**
   * return statement
   *
   *
   * val two = (x: Int) => { return x; 2 }
   *
   *
   * def sumItUp: Int = {
   *    def one(x: Int): Int = { return x; 1 }
   *    val two = (x: Int) => { return x; 2 }
   *    1 + one(2) + two(3)
   * }
   */



}
