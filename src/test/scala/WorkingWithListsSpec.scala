import java.util.NoSuchElementException

import tags._
import utils.UnitSpec

class WorkingWithListsSpec extends UnitSpec {
  val defaultIntegerList = List(1, 2, 3, 4, 5)
  val defaultUnflattenIntegerList = List(List(1), List(2, 3), List(4, 5))
  val defaultComplexUnflattenIntegerList = List(1, List(2, List(3, List(4, List(5)))))
  val defaultDuplicateInteger = List(1, 2, 2, 2, 3, 6, 4, 3, 5, 4, 4, 1)
  val defaultStringList = List("Hello", " ", "World", " ", "!")
  val palindromeOddIntegerList = List(1, 2, 3, 4, 5, 4, 3, 2, 1)
  val palindromeEvenIntegerList = List(1, 2, 2, 1)
  val oneElementList = List(1)

  "Any list" must "find the last element" taggedAs PO1 in {
    assert(WorkingWithLists.P01_findLastElement(defaultIntegerList) === 5)
    assert(WorkingWithLists.P01_findLastElement(oneElementList) === 1)
    assert(WorkingWithLists.P01_findLastElement(defaultStringList) === "!")
  }
  "PO1 An invalid list" must "throw an exception" taggedAs PO1 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P01_findLastElement(List.empty)
      WorkingWithLists.P01_findLastElement(null)
    }
  }

  "Any list" must "find the last but one element" taggedAs PO2 in {
    assert(WorkingWithLists.P02_findLastButOneElement(defaultIntegerList) === 4)
    assert(WorkingWithLists.P02_findLastButOneElement(oneElementList) === 1)
    assert(WorkingWithLists.P02_findLastButOneElement(defaultStringList) === " ")
  }
  "PO2 An invalid list" must "trow an exception" taggedAs PO2 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P02_findLastButOneElement(List.empty)
      WorkingWithLists.P02_findLastButOneElement(null)
    }
  }

  "Any list" must "find the Xth element" taggedAs PO3 in {
    assert(WorkingWithLists.P03_findXthElement(2, defaultIntegerList) === 3)
    assert(WorkingWithLists.P03_findXthElement(0, oneElementList) === 1)
    assert(WorkingWithLists.P03_findXthElement(2, defaultStringList) === "World")
  }
  "PO3 An invalid list" must "trow an exception" taggedAs PO3 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P03_findXthElement(2, List.empty)
      WorkingWithLists.P03_findXthElement(2, null)
    }
  }
  "PO3 An illegal index" must "trow an exception" taggedAs PO3 in {
    assertThrows[IllegalArgumentException] {
      WorkingWithLists.P03_findXthElement(42, defaultIntegerList)
      WorkingWithLists.P03_findXthElement(defaultIntegerList.length, defaultIntegerList)
      WorkingWithLists.P03_findXthElement(-1, defaultIntegerList)
    }
  }

  "Any list" must "get length" taggedAs PO4 in {
    assert(WorkingWithLists.P04_getLength(defaultIntegerList) === 5)
    assert(WorkingWithLists.P04_getLength(oneElementList) === 1)
    assert(WorkingWithLists.P04_getLength(defaultStringList) === 5)
  }
  "PO4 An invalid list" must "trow an exception" taggedAs PO4 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P04_getLength(List.empty)
      WorkingWithLists.P04_getLength(null)
    }
  }

  "Any list" must "be reverse" taggedAs PO5 in {
    assert(WorkingWithLists.P05_reverse(defaultIntegerList) === List(5, 4, 3, 2, 1))
    assert(WorkingWithLists.P05_reverse(oneElementList) === List(1))
    assert(WorkingWithLists.P05_reverse(defaultStringList) === List("!", " ", "World", " ", "Hello"))
  }
  "PO5 An invalid list" must "trow an exception" taggedAs PO5 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P05_reverse(List.empty)
      WorkingWithLists.P05_reverse(null)
    }
  }

  "Any list" must "be palindrome" taggedAs PO6 in {
    assert(WorkingWithLists.P06_isPalindrome(defaultIntegerList) === false)
    assert(WorkingWithLists.P06_isPalindrome(oneElementList) === true)
    assert(WorkingWithLists.P06_isPalindrome(defaultStringList) === false)
    assert(WorkingWithLists.P06_isPalindrome(palindromeOddIntegerList) === true)
    assert(WorkingWithLists.P06_isPalindrome(palindromeEvenIntegerList) === true)
  }
  "PO6 An invalid list" must "trow an exception" taggedAs PO6 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P06_isPalindrome(List.empty)
      WorkingWithLists.P06_isPalindrome(null)
    }
  }

  "Any list" must "be flatten" taggedAs PO7 in {
    assert(WorkingWithLists.PO7_flatten(defaultUnflattenIntegerList) === defaultIntegerList)
    assert(WorkingWithLists.PO7_flatten(defaultComplexUnflattenIntegerList) === defaultIntegerList)
  }
  "PO7 An invalid list" must "trow an exception" taggedAs PO7 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.PO7_flatten(List.empty)
      WorkingWithLists.PO7_flatten(null)
    }
  }

  "Any list" must "removed duplicate" taggedAs PO8 in {
    assert(WorkingWithLists.PO8_removeDuplicate(defaultDuplicateInteger) === List(1, 2 , 3, 6, 4, 5))
    assert(WorkingWithLists.PO8_removeDuplicate(oneElementList) === List(1))
  }
  "PO8 An invalid list" must "trow an exception" taggedAs PO8 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.PO8_removeDuplicate(List.empty)
      WorkingWithLists.PO8_removeDuplicate(null)
    }
  }
}
