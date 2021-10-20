package edu.knoldus

class Searching {

  def binarySearch(array: Array[Int], elem: Int): Boolean = {

    @scala.annotation.tailrec
    def binarySearchHelper(array: Array[Int],
                           elem: Int,
                           low: Int,
                           high: Int): Boolean = if (low > high) { false }
                           else {
                             val middle = low + (high - low) / 2
                             array match {
                               case array if array(middle) == elem => true
                               case array if array(middle) < elem => binarySearchHelper(array, elem, middle + 1, high)
                               case array if array(middle) > elem =>
                                 binarySearchHelper(array, elem, low, middle - 1)
                             }
                           }
    binarySearchHelper(array, elem, 0, array.length - 1)
  }

  def linearSearch(array: Array[Int], elem: Int): Boolean = {
    array match {
      case a if a.isEmpty      => false
      case a if a.head == elem => true
      case _                   => linearSearch(array.tail, elem)
    }
  }

}
