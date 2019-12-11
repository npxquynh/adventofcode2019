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
  ): List[Coordinate] = {
    val direction = Direction(move(0))
    val times = move.substring(1).toInt

    (1 to times).scanLeft(coordiate) { (coordiate, _) =>
      findNextCoordinate(coordiate, direction)
    }.tail.toList
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

object WirePath {

  val RootCoordinate = Coordinate(0, 0)

  def build(moves: Seq[String]): List[Coordinate] = {
    moves.foldLeft((List.empty[Coordinate], RootCoordinate)) { (X, move) => 
      val points = Coordinate.findAllCoordinates(X._2, move)
      (X._1 ++ points, points.last)
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
    val firstPath = WirePath.build(first).toSet
    val secondPath = WirePath.build(second).toSet

    val crossedPoints = firstPath.intersect(secondPath)
    crossedPoints.map(coordinate => DistanceFn(coordinate.x, coordinate.y)).min
  }
}
