import java.util.NoSuchElementException

object WorkingWithLists {
  def P01_findLastElement[A](initialList: List[A]): A = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.last
  }

  def P02_findLastButOneElement[A](initialList: List[A]): A = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.takeRight(2).head
  }

  def P03_findXthElement[A](indexElement: Int, initialList: List[A]): A = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    if (indexElement < 0 || indexElement > initialList.length - 1) {
      throw new IllegalArgumentException
    }
    initialList(indexElement)
  }

  def P04_getLength[A](initialList: List[A]): Int = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.length
  }

  def P05_reverse[A](initialList: List[A]): List[A] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.reverse
  }

  def P06_isPalindrome[A](initialList: List[A]): Boolean = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    val medianIndex = initialList.length % 2 match {
      case 0 => Math.round(initialList.length / 2.0).toInt
      case _ => Math.round(initialList.length / 2.0).toInt - 1
    }
    initialList.take(medianIndex) == initialList.takeRight(medianIndex).reverse
  }

  def PO7_flatten(initialList: List[Any]): List[Any] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.flatMap {
      case subList: List[_] => PO7_flatten(subList)
      case element => List(element)
    }
  }

  def PO8_removeDuplicate[A](initialList: List[A]): List[A] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.distinct
  }
}
