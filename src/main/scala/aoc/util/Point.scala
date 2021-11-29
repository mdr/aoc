package aoc.util

val OriginPoint: Point = Point(0, 0)

case class Point(x: Int, y: Int) {

    def +(direction: Direction): Point = copy(x = x + direction.x, y = y + direction.y)

}