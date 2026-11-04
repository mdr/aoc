package aoc2025.utils

import scala.collection.mutable

object AStar:

  /**
   * Find minimum cost from start to goal using A* search.
   *
   * @param start     Starting node
   * @param goal      Target node
   * @param neighbors Function returning neighbors with edge costs
   * @param heuristic Admissible heuristic estimating cost to goal.
   *                  Return Double.PositiveInfinity to indicate a node cannot reach the goal.
   *                  Default is 0 (equivalent to Dijkstra's algorithm).
   * @return Minimum cost if path exists, None otherwise
   */
  def findMinCost[T](
      start: T,
      goal: T,
      neighbors: T => Seq[(T, Double)],
      heuristic: T => Double = (_: T) => 0.0
  ): Option[Double] =
    // Priority queue ordered by f-score (g + h), lower is better
    val openSet = mutable.PriorityQueue.empty[(Double, T)](using Ordering.by[(Double, T), Double](_._1).reverse)

    // Best known cost to reach each node (g-score)
    val gScore = mutable.Map[T, Double]().withDefaultValue(Double.PositiveInfinity)

    gScore(start) = 0.0
    openSet.enqueue((heuristic(start), start))

    while openSet.nonEmpty do
      val (_, current) = openSet.dequeue()

      if current == goal then return Some(gScore(current))

      val currentG = gScore(current)

      // Skip if we've already found a better path to this node
      if currentG != Double.PositiveInfinity then
        for (neighbor, edgeCost) <- neighbors(current) do
          val tentativeG = currentG + edgeCost
          if tentativeG < gScore(neighbor) then
            gScore(neighbor) = tentativeG
            val h = heuristic(neighbor)
            if h != Double.PositiveInfinity then
              val fScore = tentativeG + h
              openSet.enqueue((fScore, neighbor))

    None
