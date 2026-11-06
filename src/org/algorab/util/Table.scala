package org.algorab.util

import io.github.iltotore.iron.RefinedType

case class Table[K, V](entries: Map[K, V], next: K)(using keyCap: Table.Key[K]):

  def apply(key: K): V = entries(key)

  def withValue(value: V): Table[K, V] = this.copy(
    entries = entries.updated(next, value),
    next = keyCap.next(next)
  )

  def withEntry(key: K, value: V): Table[K, V] = this.copy(
    entries = entries.updated(key, value),
    next = keyCap.max(key, next)
  )

object Table:

  trait Key[K]:
    def next(last: K): K
    def max(a: K, b: K): K

  object Key:

    given Key[Int] with
      override def next(last: Int): Int = last + 1
      override def max(a: Int, b: Int): Int = a.max(b)

    def refined[T](using m: RefinedType.Mirror[T], baseKey: Key[m.BaseType]): Key[T] = new:
      override def next(last: T): T = baseKey.next(last.asInstanceOf[m.BaseType]).asInstanceOf[T]
      override def max(a: T, b: T): T = baseKey.max(a.asInstanceOf[m.BaseType], b.asInstanceOf[m.BaseType]).asInstanceOf[T]
