package xyz.hyperreal.bvm

import scala.collection.mutable.ArrayBuffer


class ArrayBufferStack[T] extends ArrayBuffer[T] {

  def push( e: T* ) = appendAll( e )

  def pop = remove( size - 1 )

  def pop( n: Int ) = {
    val res = slice( size - n, size ).reverse.toList

    discard( n )
    res
  }

  def top = last

  def discard( n: Int ) = remove( size - n, n )

  def swap = push( pop(2): _* )

  def search( p: T => Boolean ) = reverseIterator find p
}