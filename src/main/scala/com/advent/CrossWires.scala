package com.advent

import com.advent.Direction._
import scala.collection.mutable
import scala.collection.SetLike
import scala.collection.mutable.{Builder, SetBuilder}
import scala.collection.generic.{
  GenericSetTemplate,
  GenericCompanion,
  CanBuildFrom
}
import scala.math

sealed class Direction
object Direction {
  case object RIGHT extends Direction
  case object LEFT extends Direction
  case object DOWN extends Direction
  case object UP extends Direction

  def apply(char: Char): Direction = char match {
    case 'R' => RIGHT
    case 'L' => LEFT
    case 'D' => DOWN
    case 'U' => UP
    case _ =>
      throw new IllegalArgumentException(s"Direction '$char' is invalid.")
  }
}

case class Coordinate(x: Int, y: Int)

object Coordinate {

  def findAllCoordinates(
      coordiate: Coordinate,
      move: String
  ): Seq[Coordinate] = {
    val direction = Direction(move(0))
    val times = move.substring(1).toInt

    (1 to times).scanLeft(coordiate) { (coordiate, _) =>
      findNextCoordinate(coordiate, direction)
    }
  }

  private def findNextCoordinate(
      currentCoordinate: Coordinate,
      direction: Direction
  ): Coordinate = direction match {
    // Catersian
    case DOWN  => currentCoordinate.copy(y = currentCoordinate.y - 1)
    case UP    => currentCoordinate.copy(y = currentCoordinate.y + 1)
    case RIGHT => currentCoordinate.copy(x = currentCoordinate.x + 1)
    case LEFT  => currentCoordinate.copy(x = currentCoordinate.x - 1)
  }
}

class WirePath(seq: Coordinate*)
    extends Set[Coordinate]
    with SetLike[Coordinate, WirePath]
    with Serializable {

  override def empty: WirePath = new WirePath()

  // Members declared in scala.collection.GenSetLike
  def iterator: Iterator[Coordinate] = seq.iterator

  // Members declared in scala.collection.SetLike
  def -(elem: Coordinate): WirePath =
    if (!seq.contains(elem)) this
    else new WirePath(seq.filterNot(elem ==): _*)

  def +(elem: Coordinate): WirePath =
    if (seq.contains(elem)) this
    else {
      new WirePath(elem +: seq: _*)
    }

  def contains(elem: Coordinate): Boolean = seq exists (elem ==)
}

object WirePath {

  val RootCoordinate = Coordinate(0, 0)
  val Start = new WirePath()

  // SetLike
  def empty: WirePath = new WirePath()
  def newBuilder: Builder[Coordinate, WirePath] =
    new SetBuilder[Coordinate, WirePath](empty)
  def apply(elems: Coordinate*): WirePath = (empty /: elems)(_ + _)
  def thingSetCanBuildFrom = new CanBuildFrom[WirePath, Coordinate, WirePath] {
    def apply(from: WirePath) = newBuilder
    def apply() = newBuilder
  }

  def build(moves: Seq[String]): WirePath = {
    moves.foldLeft((Start, RootCoordinate)) { (X, move) => 
      {
        val paths = Coordinate.findAllCoordinates(X._2, move)
        val wirePath = X._1 ++ (new WirePath(paths.tail: _*))  
        (wirePath, paths.last)
      }
    }._1
  }

}

object CrossWires {

  private val manhattanDistance: (Int, Int) => Int = (x, y) => math.abs(x) + math.abs(y)
  val DistanceFn = manhattanDistance

  /**
   * What is the Manhattan distance from the central port to the closest intersection?
   */
  def execute(first: Seq[String], second: Seq[String]): Int = {
    val firstPath = WirePath.build(first)
    val secondPath = WirePath.build(second)

    val crossedPoints = firstPath.intersect(secondPath)
    crossedPoints.map(coordinate => DistanceFn(coordinate.x, coordinate.y)).min
  }
}
