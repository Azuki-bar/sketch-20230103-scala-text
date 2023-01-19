package object collect {
  def swapArray[T](arr: Array[T])(i: Int, j: Int): Unit = {
    val tmp = arr(i)
    arr(i) = arr(j)
    arr(j) = tmp
  }
  def joinByComma(start: Int, end: Int): String = {
    (start to end).mkString(",")
  }
  def reverse[T](list: List[T]): List[T] =
    list.foldLeft(Nil: List[T])((a, b) => b :: a)

  def sum(list: List[Int]): Int = {
    list.foldRight(0)((a, b) => a + b)
  }

  def mkString[T](list: List[T])(sep: String): String = {
    list match {
      case Nil => ""
      case head :: next =>
        next.foldLeft(head.toString) { (x, y) => x + sep + y }
    }
  }

  def map[T, U](list: List[T])(f: T => U): List[U] = {
    list
      .foldLeft(Nil: List[U]) { (x, y) => f(y) :: x }
      .reverse
  }

  def filter[T](list: List[T])(f: T => Boolean): List[T] = {
    list
      .foldLeft(Nil: List[T]) { (x, y) =>
        (x, f(y)) match {
          case (_, true)  => y :: x
          case (_, false) => x
        }
      }
      .reverse
  }

  def find[T](list: List[T])(f: T => Boolean): Option[T] = {
    list match {
      case Nil                     => None
      case head :: next if f(head) => Option(head)
      case head :: next            => find(next)(f)
    }
  }

  def takeWhile[T](list: List[T])(f: T => Boolean): List[T] = {
    list match {
      case head :: next if f(head) => head :: takeWhile(next)(f)
      case _                       => Nil
    }
  }

  def count[T](list: List[T])(f: T => Boolean): Int = {
    list.foldLeft(0) { (pre, cur) => if (f(cur)) pre + 1 else pre }
  }

  def flatMap[T, U](list: List[T])(f: T => List[U]): List[U] = {
    list match {
      case head :: next => f(head) ::: flatMap(next)(f)
      case Nil          => Nil
    }
  }
}
