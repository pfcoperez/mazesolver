package org.pfcoperez.mazesolver.datastructures

import cats.collections.DisjointSets

trait DisjointSetsWrapper[T] {
  type Imp[_]

  val underlying: Imp[T]

  def union(a: T, b: T): (DisjointSetsWrapper[T], Boolean)
  def contains(v: T): Boolean
  def find(v: T): (DisjointSetsWrapper[T], Option[T])
  def +(v: T): DisjointSetsWrapper[T]
  def toSets: (DisjointSetsWrapper[T], Map[T, Set[T]])

  val description: String
}

object DisjointSetsWrapper {
  import cats.Order
  implicit def catsOrderFromOrdering[T](implicit
      ordering: Ordering[T]
  ): Order[T] = Order.fromOrdering(ordering)

  case class CatsDisjointSets[T: Ordering](underlying: DisjointSets[T])
      extends DisjointSetsWrapper[T] {

    type Imp[_] = DisjointSets[T]

    def union(a: T, b: T): (DisjointSetsWrapper[T], Boolean) = {
      val (updatedUnderlying, result) = underlying.union(a, b)
      CatsDisjointSets(updatedUnderlying) -> result
    }

    def contains(v: T): Boolean = underlying.contains(v)

    def find(v: T): (DisjointSetsWrapper[T], Option[T]) = {
      val (updatedUnderlying, result) = underlying.find(v)
      CatsDisjointSets(updatedUnderlying) -> result
    }

    def +(v: T): DisjointSetsWrapper[T] = {
      CatsDisjointSets(underlying + v)
    }

    def toSets: (DisjointSetsWrapper[T], Map[T, Set[T]]) = {
      val (updatedUnderlying, result) = underlying.toSets
      CatsDisjointSets(updatedUnderlying) -> result.toScalaMap.mapValues(
        _.toScalaSet
      )
    }

    val description: String = "cats-collection DisjointSets implementation"
  }

  case class FakeDisjointSets[T: Ordering](
      underlying: collection.mutable.Map[T, collection.mutable.Set[T]]
  ) extends DisjointSetsWrapper[T] {

    type Imp[_] = collection.mutable.Map[T, collection.mutable.Set[T]]

    def union(a: T, b: T): (DisjointSetsWrapper[T], Boolean) = {
      val maybeUpdated = for {
        aParent <- find(a)._2
        bParent <- find(b)._2
        aSet <- underlying.get(aParent)
        bSet <- underlying.get(bParent)
      } yield {
        val joined = aSet.union(bSet)
        joined.add(bParent)
        underlying.update(aParent, joined)
        underlying.remove(bParent)
        true
      }
      this -> maybeUpdated.getOrElse(false)
    }

    def contains(v: T): Boolean = {
      underlying.contains(v) || {
        val (_, findResult) = find(v)
        findResult.isDefined
      }
    }

    def find(v: T): (DisjointSetsWrapper[T], Option[T]) = {
      val result = underlying.collectFirst {
        case (parent, set) if set.contains(v) => parent
      }
      this -> result
    }

    def +(v: T): DisjointSetsWrapper[T] = {
      if (!contains(v)) {
        underlying.update(v, collection.mutable.Set.empty)
      }
      this
    }

    def toSets: (DisjointSetsWrapper[T], Map[T, Set[T]]) = {
      this -> underlying.toMap.mapValues(_.toSet)
    }

    val description: String =
      "Mutable map of mutable sets DisjointSets implementation"
  }

  def createCatsBacked[T: Ordering](labels: Seq[T]) = {
    CatsDisjointSets(DisjointSets(labels: _*))
  }

  def createMutableBacked[T: Ordering](labels: Seq[T]) = {
    FakeDisjointSets(
      collection.mutable.Map(
        labels.map(l => l -> collection.mutable.Set(l)): _*
      )
    )
  }
}
