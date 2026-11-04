package aoc.util

import aoc.util.*
import scala.annotation.tailrec
import de.sciss.fingertree._
import java.util.Date

class DijkstraContext[T](initialNode: T, destinationNode: T, nodes: Set[T], adjacency: T => Seq[(T, Double)]):

  def run: Double = run(initialState)

  @tailrec
  private def run(state: State): Double =
    state.checkFinished match
      case Some(distance) => distance
      case None           => run(state.step)

  case class NodeAndDistance(node: T, distance: Double) extends Ordered[NodeAndDistance]:
    override def compare(that: NodeAndDistance): Int = -(this.distance compare that.distance)

  case class State(
      unvisitedNodes: Set[T],
      nodeQueue: PriorityQueue[NodeAndDistance],
      tentativeDistances: Map[T, Double]
  ):

    def checkFinished: Option[Double] = tentativeDistances(destinationNode) match
      case Double.PositiveInfinity => None
      case distance                => Some(distance)

    def step: State =
      val (NodeAndDistance(currentNode, currentDistance), newNodeQueue) = nodeQueue.dequeue
      if (!(unvisitedNodes contains currentNode)) {
        return copy(nodeQueue = newNodeQueue)
      }
      val tentativeDistanceUpdates =
        adjacency(currentNode).collect {
          case (neighbour, neighbourDistance) if unvisitedNodes contains neighbour =>
            val distanceThroughCurrentNode = currentDistance + neighbourDistance
            neighbour -> (tentativeDistances(neighbour) min distanceThroughCurrentNode)
        }.toMap
      val newNewNodeQueue = tentativeDistanceUpdates.toSeq.foldLeft(newNodeQueue) { case (queue, (node, distance)) =>
        queue.enqueue(NodeAndDistance(node, distance))
      }
      State(
        unvisitedNodes - currentNode,
        newNewNodeQueue,
        tentativeDistances ++ tentativeDistanceUpdates
      )

  def initialState: State =
    val tentativeDistances = nodes.map(_ -> Double.PositiveInfinity).toMap + (initialNode -> 0.0)
    val nodeQueue =
      tentativeDistances.toSeq.foldLeft(PriorityQueue.empty[NodeAndDistance]) { case (queue, (node, distance)) =>
        queue.enqueue(NodeAndDistance(node, distance))
      }
    State(
      unvisitedNodes = nodes,
      nodeQueue = nodeQueue,
      tentativeDistances = tentativeDistances
    )
