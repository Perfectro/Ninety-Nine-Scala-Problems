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

  def P07_flatten(initialList: List[Any]): List[Any] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.flatMap {
      case subList: List[_] => P07_flatten(subList)
      case element => List(element)
    }
  }

  def P08_removeConsecutiveDuplicate[A](initialList: List[A]): List[A] = {
    def addWithoutConsecutiveDuplicate(accumulator: List[A], element: A): List[A] = {
      if (accumulator.nonEmpty && accumulator.head == element) {
        accumulator
      } else {
        element :: accumulator
      }
    }
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.foldRight(List[A]())((element, accumulator) => addWithoutConsecutiveDuplicate(accumulator, element))
  }

  def P09_packConsecutiveDuplicate[A](initialList: List[A]): List[List[A]] = {
    def addConsecutiveDuplicate(accumulator: List[List[A]], element: A): List[List[A]] = {
      if (accumulator.nonEmpty && accumulator.head.nonEmpty && accumulator.head.head == element) {
        (element :: accumulator.head) :: accumulator.drop(1)
      } else {
        List(element) :: accumulator
      }
    }
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.foldRight(List[List[A]]())((element, accumulator) => addConsecutiveDuplicate(accumulator, element))
  }

  def P10_runLengthEncoding[A](initialList: List[A]): List[(Int, A)] = {
    P09_packConsecutiveDuplicate(initialList).map(element => (element.length, element.head))
  }

  def P11_runLengthEncodingUpdated[A](initialList: List[A]): List[Any] = {
    P09_packConsecutiveDuplicate(initialList).map(
      element => {
        element.length match {
          case 1 => element.head
          case _ => (element.length, element.head)
        }
      }
    )
  }

  def P12_decodeRunLengthEncoding[A](initialList: List[(Int, A)]): List[A] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.flatMap(element => List.fill(element._1)(element._2))
  }

  def P13_runLengthEncodingStandAlone[A](initialList: List[A]): List[(Int, A)] = {
    def packConsecutiveDuplicate(accumulator: List[(Int, A)], element: A): List[(Int, A)] = {
      if (accumulator.nonEmpty && accumulator.head._2 == element) {
        (accumulator.head._1 + 1, element) :: accumulator.drop(1)
      } else {
        (1, element) :: accumulator
      }
    }
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.foldRight(List[(Int, A)]())((element, accumulator) => packConsecutiveDuplicate(accumulator, element))
  }

  def P14_duplicateListElement[A](initialList: List[A]): List[A] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.flatMap(element => List.fill(2)(element))
  }

  def P15_duplicateListElementNth[A](sizeOfDuplicate: Int, initialList: List[A]): List[A] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    initialList.flatMap(element => List.fill(sizeOfDuplicate)(element))
  }

  def P16_dropEveryNthElement[A](dropStep: Int, initialList: List[A]): List[A] = {
    def fillWithoutNthElement(dropStep: Int, accumulator: List[A], element: (A, Int)): List[A] = {
      if ((element._2 + 1) % dropStep == 0) {
        accumulator
      } else {
        element._1 :: accumulator
      }
    }
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    } else if (dropStep < 1) {
      throw new IllegalArgumentException
    } else if (dropStep == 1) {
      List.empty
    } else {
      initialList.zipWithIndex.foldRight(List[A]())((element, accumulator) => fillWithoutNthElement(dropStep, accumulator, element))
    }
  }

  def P17_splitListInTwo[A](splitIndex: Int, initialList: List[A]): (List[A], List[A]) = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    } else if (splitIndex < 0) {
      throw new IllegalArgumentException
    }
    initialList.splitAt(splitIndex)
  }

  def P18_extractSlice[A](startSlice: Int, endSlice: Int, initialList: List[A]): List[A] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    } else if (startSlice < 0 || endSlice < 0) {
      throw new IllegalArgumentException
    }
    initialList.slice(startSlice, endSlice)
  }

  def P19_rotateNthElement[A](rotation: Int, initialList: List[A]): List[A] = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    }
    val finalRotation = Math.abs(rotation) % initialList.length
    if (rotation < 0) {
      initialList.takeRight(finalRotation) ::: initialList.dropRight(finalRotation)
    } else {
      initialList.drop(finalRotation) ::: initialList.take(finalRotation)
    }
  }

  def P20_removeNthElement[A](indexToBeRemoved: Int, initialList: List[A]): (List[A], A) = {
    if (initialList == null || initialList.isEmpty) {
      throw new NoSuchElementException
    } else if (indexToBeRemoved < 0 || indexToBeRemoved >= initialList.length) {
      throw new IllegalArgumentException
    }
    val listWithIndex = initialList.zipWithIndex
    (listWithIndex.filter(_._2 != indexToBeRemoved).map(_._1), listWithIndex.filter(_._2 == indexToBeRemoved).head._1)
  }
}
