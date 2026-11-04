package aoc2015

import aoc.util.*
import java.security.MessageDigest
import scala.collection.immutable.LazyList

val secretKey1 = loadString("aoc2015/input-2015-4.txt").trim

def day04Part1: Int = mine(secretKey1, startsWithFiveZeroes)

val md5Digest = MessageDigest.getInstance("MD5")

def md5(s: String): Array[Byte] = md5Digest.digest(s.getBytes())

def startsWithFiveZeroes(hash: Array[Byte]): Boolean = hash(0) == 0 && hash(1) == 0 && (hash(2) & 0xff) < 16

def startsWithSixZeroes(hash: Array[Byte]): Boolean = hash(0) == 0 && hash(1) == 0 && hash(2) == 0

def producesMatchingHash(secretKey: String, isHit: Array[Byte] => Boolean)(n: Int): Boolean = isHit(md5(secretKey + n))

def mine(secretKey: String, isHit: Array[Byte] => Boolean): Int = Iterator.from(1).find(producesMatchingHash(secretKey, isHit)).get

def toHex(bytes: Array[Byte]): String = bytes.map("%02x".format(_)).mkString

def day04Part2: Int = mine(secretKey1, startsWithSixZeroes)