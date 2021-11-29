package aoc2015

import aoc.util.*

val boxes = loadLines("aoc2015/input-2015-2.txt").map(parseBox)

def day02Part1: Int = boxes.sumBy(_.wrappingPaperRequired)

case class Box(length: Int, width: Int, height: Int):
  def area1: Int       = length * width
  def area2: Int       = width * height
  def area3: Int       = height * length
  def surfaceArea: Int = 2 * area1 + 2 * area2 + 2 * area3

  def wrappingPaperRequired: Int = surfaceArea + List(area1, area2, area3).min

  def perimeter1: Int     = 2 * (length + width)
  def perimeter2: Int     = 2 * (width + height)
  def perimeter3: Int     = 2 * (height + length)
  def volume: Int         = length * width * height
  def ribbonRequired: Int = List(perimeter1, perimeter2, perimeter3).min + volume

def parseBox(s: String): Box =
  val List(length, width, height) = s.split("x").toList
  Box(length.toInt, width.toInt, height.toInt)

def day02Part2: Int = boxes.sumBy(_.ribbonRequired)
