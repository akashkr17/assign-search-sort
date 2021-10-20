package edu.knoldus
class Sorting {

  def insertionSort(array: Array[Int]): Array[Int] = {

    val size = array.length
    for (step <- 1 until size) {
      val key = array(step)
      var j = step - 1
      while ({
        j >= 0 && key < array(j)
      }) {
        array(j + 1) = array(j)
        j -= 1
      }
      array(j + 1) = key
    }
    array
  }

  def selectionSort(array: Array[Int],
                    first: Int = 0,
                    second: Int = 0): Array[Int] = {

    if (first >= array.length - 1) {
      array
    } else if (second >= array.length - 1) {
      selectionSort(array, first + 1)
    } else {
      for (j <- first until array.length) {
        if (array(j) < array(second)) {
          val temp = array(second)
          array(second) = array(j)
          array(j) = temp
        }
      }
      selectionSort(array, first, second + 1)
    }
    array
  }
  def bubbleSort(array: Array[Int]): Array[Int] = {
    for (i <- 0 until array.length - 1; j <- 0 until array.length - 1 - i) {
      if (array(j) > array(j + 1)) {
        val tempElem = array(j)
        array(j) = array(j + 1)
        array(j + 1) = tempElem
      }
    }
    array
  }
}
