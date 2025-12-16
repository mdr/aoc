package aoc2025.dag

case class Dag[V](private val edges: Map[V, Set[V]] = Map.empty):
  def addVertex(vertex: V): Dag[V] =
    if edges.contains(vertex) then this
    else Dag(edges + (vertex -> Set.empty))

  def addEdge(from: V, to: V): Dag[V] =
    require(!getAllDescendants(to).contains(from), s"Adding edge from $from to $to would create a cycle")
    this.addVertex(from).addVertex(to).insertEdge(from, to)

  private def insertEdge(from: V, to: V): Dag[V] = Dag(edges.updated(from, edges(from) + to))

  def getAllDescendants(vertex: V): Set[V] = edges(vertex).flatMap(child => getAllDescendants(child) + child)
