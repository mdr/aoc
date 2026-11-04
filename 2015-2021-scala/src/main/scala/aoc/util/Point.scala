package aoc.util

val OriginPoint: Point = Point(0, 0)

object Point {
    val Origin: Point = Point(0, 0)
}

case class Point(x: Long, y: Long) {

    def +(direction: Direction): Point = copy(x = x + direction.x, y = y + direction.y)

    def +(point: Point): Point = copy(x = x + point.x, y = y + point.y)
}