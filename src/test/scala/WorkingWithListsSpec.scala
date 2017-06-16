import java.util.NoSuchElementException

import tags._
import utils.UnitSpec

class WorkingWithListsSpec extends UnitSpec {
  val defaultIntegerList = List(1, 2, 3, 4, 5)
  val defaultUnflattenIntegerList = List(List(1), List(2, 3), List(4, 5))
  val defaultComplexUnflattenIntegerList = List(1, List(2, List(3, List(4, List(5)))))
  val defaultDuplicateInteger = List(1, 2, 2, 2, 3, 6, 6, 4, 3, 3, 3, 6, 6, 5, 4, 4, 1)
  val defaultRunLengthEncoding = List((1, 1), (3, 2), (1, 3), (2, 6), (1, 4), (3, 3), (2, 6), (1, 5), (2, 4), (1, 1))
  val defaultStringList = List("Hello", " ", "World", " ", "!")
  val palindromeOddIntegerList = List(1, 2, 3, 4, 5, 4, 3, 2, 1)
  val palindromeEvenIntegerList = List(1, 2, 2, 1)
  val oneElementList = List(1)
  val oneRunLengthEncodingElementList = List((1, 1))

  "Any list" must "find the last element" taggedAs P01 in {
    assert(WorkingWithLists.P01_findLastElement(defaultIntegerList) === 5)
    assert(WorkingWithLists.P01_findLastElement(oneElementList) === 1)
    assert(WorkingWithLists.P01_findLastElement(defaultStringList) === "!")
  }
  "P01 An invalid list" must "throw an exception" taggedAs P01 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P01_findLastElement(List.empty)
      WorkingWithLists.P01_findLastElement(null)
    }
  }

  "Any list" must "find the last but one element" taggedAs P02 in {
    assert(WorkingWithLists.P02_findLastButOneElement(defaultIntegerList) === 4)
    assert(WorkingWithLists.P02_findLastButOneElement(oneElementList) === 1)
    assert(WorkingWithLists.P02_findLastButOneElement(defaultStringList) === " ")
  }
  "P02 An invalid list" must "trow an exception" taggedAs P02 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P02_findLastButOneElement(List.empty)
      WorkingWithLists.P02_findLastButOneElement(null)
    }
  }

  "Any list" must "find the Xth element" taggedAs P03 in {
    assert(WorkingWithLists.P03_findXthElement(2, defaultIntegerList) === 3)
    assert(WorkingWithLists.P03_findXthElement(0, oneElementList) === 1)
    assert(WorkingWithLists.P03_findXthElement(2, defaultStringList) === "World")
  }
  "P03 An invalid list" must "trow an exception" taggedAs P03 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P03_findXthElement(2, List.empty)
      WorkingWithLists.P03_findXthElement(2, null)
    }
  }
  "P03 An illegal index" must "trow an exception" taggedAs P03 in {
    assertThrows[IllegalArgumentException] {
      WorkingWithLists.P03_findXthElement(42, defaultIntegerList)
      WorkingWithLists.P03_findXthElement(defaultIntegerList.length, defaultIntegerList)
      WorkingWithLists.P03_findXthElement(-1, defaultIntegerList)
    }
  }

  "Any list" must "get length" taggedAs P04 in {
    assert(WorkingWithLists.P04_getLength(defaultIntegerList) === 5)
    assert(WorkingWithLists.P04_getLength(oneElementList) === 1)
    assert(WorkingWithLists.P04_getLength(defaultStringList) === 5)
  }
  "P04 An invalid list" must "trow an exception" taggedAs P04 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P04_getLength(List.empty)
      WorkingWithLists.P04_getLength(null)
    }
  }

  "Any list" must "be reverse" taggedAs P05 in {
    assert(WorkingWithLists.P05_reverse(defaultIntegerList) === List(5, 4, 3, 2, 1))
    assert(WorkingWithLists.P05_reverse(oneElementList) === List(1))
    assert(WorkingWithLists.P05_reverse(defaultStringList) === List("!", " ", "World", " ", "Hello"))
  }
  "P05 An invalid list" must "trow an exception" taggedAs P05 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P05_reverse(List.empty)
      WorkingWithLists.P05_reverse(null)
    }
  }

  "Any list" must "be palindrome" taggedAs P06 in {
    assert(WorkingWithLists.P06_isPalindrome(defaultIntegerList) === false)
    assert(WorkingWithLists.P06_isPalindrome(oneElementList) === true)
    assert(WorkingWithLists.P06_isPalindrome(defaultStringList) === false)
    assert(WorkingWithLists.P06_isPalindrome(palindromeOddIntegerList) === true)
    assert(WorkingWithLists.P06_isPalindrome(palindromeEvenIntegerList) === true)
  }
  "P06 An invalid list" must "trow an exception" taggedAs P06 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P06_isPalindrome(List.empty)
      WorkingWithLists.P06_isPalindrome(null)
    }
  }

  "Any list" must "be flatten" taggedAs P07 in {
    assert(WorkingWithLists.P07_flatten(defaultUnflattenIntegerList) === defaultIntegerList)
    assert(WorkingWithLists.P07_flatten(defaultComplexUnflattenIntegerList) === defaultIntegerList)
  }
  "P07 An invalid list" must "trow an exception" taggedAs P07 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P07_flatten(List.empty)
      WorkingWithLists.P07_flatten(null)
    }
  }

  "Any list" must "removed consecuvitve duplicate" taggedAs P08 in {
    assert(WorkingWithLists.P08_removeConsecutiveDuplicate(defaultDuplicateInteger) === List(1, 2 , 3, 6, 4, 3, 6, 5, 4, 1))
    assert(WorkingWithLists.P08_removeConsecutiveDuplicate(oneElementList) === List(1))
  }
  "P08 An invalid list" must "trow an exception" taggedAs P08 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P08_removeConsecutiveDuplicate(List.empty)
      WorkingWithLists.P08_removeConsecutiveDuplicate(null)
    }
  }

  "Any list" must "pack consecuvitve duplicate" taggedAs P09 in {
    assert(WorkingWithLists.P09_packConsecutiveDuplicate(defaultDuplicateInteger) === List(List(1), List(2, 2, 2), List(3), List(6, 6), List(4), List(3, 3, 3), List(6, 6), List(5), List(4, 4), List(1)))
    assert(WorkingWithLists.P09_packConsecutiveDuplicate(oneElementList) === List(List(1)))
  }
  "P09 An invalid list" must "trow an exception" taggedAs P09 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P09_packConsecutiveDuplicate(List.empty)
      WorkingWithLists.P09_packConsecutiveDuplicate(null)
    }
  }

  "Any list" must "run length encoding" taggedAs P10 in {
    assert(WorkingWithLists.P10_runLengthEncoding(defaultDuplicateInteger) === List((1, 1), (3, 2), (1, 3), (2, 6), (1, 4), (3, 3), (2, 6), (1, 5), (2, 4), (1, 1)))
    assert(WorkingWithLists.P10_runLengthEncoding(oneElementList) === List((1, 1)))
  }
  "P10 An invalid list" must "trow an exception" taggedAs P10 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P10_runLengthEncoding(List.empty)
      WorkingWithLists.P10_runLengthEncoding(null)
    }
  }

  "Any list" must "run length encoding updated" taggedAs P11 in {
    assert(WorkingWithLists.P11_runLengthEncodingUpdated(defaultDuplicateInteger) === List(1, (3, 2), 3, (2, 6), 4, (3, 3), (2, 6), 5, (2, 4), 1))
    assert(WorkingWithLists.P11_runLengthEncodingUpdated(oneElementList) === List(1))
  }
  "P11 An invalid list" must "trow an exception" taggedAs P11 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P11_runLengthEncodingUpdated(List.empty)
      WorkingWithLists.P11_runLengthEncodingUpdated(null)
    }
  }

  "Any list" must "decode run length encoding" taggedAs P12 in {
    assert(WorkingWithLists.P12_decodeRunLengthEncoding(defaultRunLengthEncoding) === List(1, 2, 2, 2, 3, 6, 6, 4, 3, 3, 3, 6, 6, 5, 4, 4, 1))
    assert(WorkingWithLists.P12_decodeRunLengthEncoding(oneRunLengthEncodingElementList) === List(1))
  }
  "P12 An invalid list" must "trow an exception" taggedAs P12 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P12_decodeRunLengthEncoding(List.empty)
      WorkingWithLists.P12_decodeRunLengthEncoding(null)
    }
  }

  "Any list" must "run length encoding stand alone" taggedAs P13 in {
    assert(WorkingWithLists.P13_runLengthEncodingStandAlone(defaultDuplicateInteger) === List((1, 1), (3, 2), (1, 3), (2, 6), (1, 4), (3, 3), (2, 6), (1, 5), (2, 4), (1, 1)))
    assert(WorkingWithLists.P13_runLengthEncodingStandAlone(oneElementList) === List((1, 1)))
  }
  "P13 An invalid list" must "trow an exception" taggedAs P13 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P13_runLengthEncodingStandAlone(List.empty)
      WorkingWithLists.P13_runLengthEncodingStandAlone(null)
    }
  }

  "Any list" must "duplicate list element" taggedAs P14 in {
    assert(WorkingWithLists.P14_duplicateListElement(defaultIntegerList) === List(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
    assert(WorkingWithLists.P14_duplicateListElement(oneElementList) === List(1, 1))
  }
  "P14 An invalid list" must "trow an exception" taggedAs P14 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P14_duplicateListElement(List.empty)
      WorkingWithLists.P14_duplicateListElement(null)
    }
  }

  "Any list" must "duplicate list element Nth" taggedAs P15 in {
    assert(WorkingWithLists.P15_duplicateListElementNth(-3, defaultIntegerList) === List())
    assert(WorkingWithLists.P15_duplicateListElementNth(0, defaultIntegerList) === List())
    assert(WorkingWithLists.P15_duplicateListElementNth(3, defaultIntegerList) === List(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5))
    assert(WorkingWithLists.P15_duplicateListElementNth(4, oneElementList) === List(1, 1, 1, 1))
  }
  "P15 An invalid list" must "trow an exception" taggedAs P15 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P15_duplicateListElementNth(1, List.empty)
      WorkingWithLists.P15_duplicateListElementNth(1, null)
    }
  }

  "Any list" must "drop every Nth element" taggedAs P16 in {
    assert(WorkingWithLists.P16_dropEveryNthElement(1, defaultIntegerList) === List())
    assert(WorkingWithLists.P16_dropEveryNthElement(2, defaultIntegerList) === List(1, 3, 5))
    assert(WorkingWithLists.P16_dropEveryNthElement(4, oneElementList) === List(1))
  }
  "P16 An invalid list" must "trow an exception" taggedAs P16 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P16_dropEveryNthElement(1, List.empty)
      WorkingWithLists.P16_dropEveryNthElement(1, null)
    }
  }
  "P16 An illegal step" must "trow an exception" taggedAs P16 in {
    assertThrows[IllegalArgumentException] {
      WorkingWithLists.P16_dropEveryNthElement(0, defaultIntegerList)
      WorkingWithLists.P16_dropEveryNthElement(-1, defaultIntegerList)
    }
  }

  "Any list" must "split in two" taggedAs P17 in {
    assert(WorkingWithLists.P17_splitListInTwo(0, defaultIntegerList) === (List(), List(1, 2, 3, 4, 5)))
    assert(WorkingWithLists.P17_splitListInTwo(1, defaultIntegerList) === (List(1), List(2, 3, 4, 5)))
    assert(WorkingWithLists.P17_splitListInTwo(2, defaultIntegerList) === (List(1, 2), List(3, 4, 5)))
    assert(WorkingWithLists.P17_splitListInTwo(4, oneElementList) === (List(1), List()))
  }
  "P17 An invalid list" must "trow an exception" taggedAs P17 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P17_splitListInTwo(1, List.empty)
      WorkingWithLists.P17_splitListInTwo(1, null)
    }
  }
  "P17 An illegal index" must "trow an exception" taggedAs P17 in {
    assertThrows[IllegalArgumentException] {
      WorkingWithLists.P17_splitListInTwo(-1, defaultIntegerList)
    }
  }

  "Any list" must "extract slice" taggedAs P18 in {
    assert(WorkingWithLists.P18_extractSlice(0, 3, defaultIntegerList) === List(1, 2, 3))
    assert(WorkingWithLists.P18_extractSlice(1, 3, defaultIntegerList) === List(2, 3))
    assert(WorkingWithLists.P18_extractSlice(2, 2, defaultIntegerList) === List())
    assert(WorkingWithLists.P18_extractSlice(2, 7, defaultIntegerList) === List(3, 4, 5))
    assert(WorkingWithLists.P18_extractSlice(42, 4, defaultIntegerList) === List())
  }
  "P18 An invalid list" must "trow an exception" taggedAs P18 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P18_extractSlice(1, 2, List.empty)
      WorkingWithLists.P18_extractSlice(1, 2, null)
    }
  }
  "P18 An illegal index" must "trow an exception" taggedAs P18 in {
    assertThrows[IllegalArgumentException] {
      WorkingWithLists.P18_extractSlice(-1, 2, defaultIntegerList)
      WorkingWithLists.P18_extractSlice(1, -2, defaultIntegerList)
      WorkingWithLists.P18_extractSlice(-1, -2, defaultIntegerList)
    }
  }

  "Any list" must "rotate Nth element" taggedAs P19 in {
    assert(WorkingWithLists.P19_rotateNthElement(0, defaultIntegerList) === defaultIntegerList)
    assert(WorkingWithLists.P19_rotateNthElement(3, defaultIntegerList) === List(4, 5, 1, 2, 3))
    assert(WorkingWithLists.P19_rotateNthElement(-3, defaultIntegerList) === List(3, 4, 5, 1, 2))
    assert(WorkingWithLists.P19_rotateNthElement(6, defaultIntegerList) === List(2, 3, 4, 5, 1))
    assert(WorkingWithLists.P19_rotateNthElement(-6, defaultIntegerList) === List(5, 1, 2, 3, 4))
  }
  "P19 An invalid list" must "trow an exception" taggedAs P19 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P19_rotateNthElement(2, List.empty)
      WorkingWithLists.P19_rotateNthElement(2, null)
    }
  }

  "Any list" must "remove Nth element" taggedAs P20 in {
    assert(WorkingWithLists.P20_removeNthElement(0, defaultIntegerList) === (List(2, 3, 4, 5), 1))
    assert(WorkingWithLists.P20_removeNthElement(3, defaultIntegerList) === (List(1, 2, 3, 5), 4))
  }
  "P20 An invalid list" must "trow an exception" taggedAs P20 in {
    assertThrows[NoSuchElementException] {
      WorkingWithLists.P20_removeNthElement(2, List.empty)
      WorkingWithLists.P20_removeNthElement(2, null)
    }
  }
  "P20 An illegal index" must "trow an exception" taggedAs P20 in {
    assertThrows[IllegalArgumentException] {
      WorkingWithLists.P20_removeNthElement(42, defaultIntegerList)
      WorkingWithLists.P20_removeNthElement(-2, defaultIntegerList)
      WorkingWithLists.P20_removeNthElement(5, defaultIntegerList)
    }
  }
}
