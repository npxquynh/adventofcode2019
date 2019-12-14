package com.advent

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

case class SpaceObject(val name: String) {
  var children = ListBuffer[SpaceObject]()
  var distanceFromCenterOfMass: Int = 0

  def addChild(child: SpaceObject) = {
    children += child
  }
}

/**
  * Earth)Moon
  * Sun)Earth
  * Sun)Mercury
  * Sun)Saturn
  * Saturn)Titan
  * Sun)Jupyter
  * Jupyter)Io
  * Jupyter)Europa
  *
  * Moon -> Earth
  * Earth -> Sun
  */
class UniversalOrbitMap(val universe: Map[String, SpaceObject]) {

  import UniversalOrbitMap._

  def countOrbits(): Int = {
    def updateChildrenOf(name: String): Unit = {
      val currentObject = universe.get(name)
      assert(currentObject.nonEmpty, s"Object with '$name' cannot be found")

      val children = currentObject.get.children
      children.foreach { child =>
        {
          child.distanceFromCenterOfMass = currentObject.get.distanceFromCenterOfMass + 1
          updateChildrenOf(child.name)
        }
      }
    }

    updateChildrenOf(CenterOfMass.name)
    universe.values.foldLeft(0) { (orbitCount, spaceObject) =>
      orbitCount + spaceObject.distanceFromCenterOfMass
    }
  }

  def findMinimumOrbitalTransferBetween(x: String, y: String): Int = {
    type Path = List[SpaceObject]
    val EmptyPath = List[SpaceObject]()

    /**
      * Returns path from "start" to "end" if it exists
      */
    def dfs1(
        start: SpaceObject,
        end: SpaceObject,
        path: Path
    ): Path = {
      val newPath = start :: path
      if (newPath.contains(end)) 
        return newPath
      else {
        val neighbours: List[SpaceObject] = universe
          .get(start.name)
          .get
          .children
          .toList filterNot newPath.contains
        neighbours.foreach { n =>
          val x = dfs1(n, end, newPath)
          if (x.nonEmpty) return x
        }
      }
      List.empty[SpaceObject]
    }

    /**
     * Outputs the remaining path since the point of diversion.
     * 
     * pathA: [A, B, C, D, E]
     * pathB: [A, B, F, G]
     * 
     * output: [[C, D, E], [F, G]]
     */
    def extractDivergingPaths(pathA: Path, pathB: Path): (Path, Path) = (pathA, pathB) match {
      case (headA :: tailA, headB :: tailB) if (headA == headB) => extractDivergingPaths(tailA, tailB)
      case (headA :: _, headB :: _) if (headA != headB) => (pathA, pathB)
      case (Nil, _) => (EmptyPath, pathB)
      case (_, Nil) => (pathA, EmptyPath)
    }
    val start = universe.get(x)
    assert(start.nonEmpty, s"Object '$x' cannot be found")
    val end = universe.get(y)
    assert(end.nonEmpty, s"Object '$y' cannot be found")

    val pathToStart = dfs1(CenterOfMass, start.get, List()).reverse
    val pathToEnd = dfs1(CenterOfMass, end.get, List()).reverse
    val divergingPaths = extractDivergingPaths(pathToStart, pathToEnd)
    divergingPaths._1.size + divergingPaths._2.size - 2
  }
}

object UniversalOrbitMap {

  val CenterOfMass = SpaceObject("COM")

  def apply(input: Seq[(String, String)]): UniversalOrbitMap = {
    val universe = Map[String, SpaceObject]("COM" -> CenterOfMass)

    input.foreach {
      case (center, satellite) => {
        val spaceObjectCenter =
          universe.getOrElseUpdate(center, SpaceObject(center))
        val spaceObjectSatellite =
          universe.getOrElseUpdate(satellite, SpaceObject(satellite))

        spaceObjectCenter.addChild(spaceObjectSatellite)
      }
    }

    new UniversalOrbitMap(universe)
  }
}
