package aoc2015

import aoc.util.*

val strings = loadLines("aoc2015/input-2015-5.txt")

def isVowel(c: Char): Boolean = c == 'a' || c == 'e' || c == 'i' ||  c == 'o' || c == 'u'

def containsAtLeastThreeVowels(s: String): Boolean = s.count(isVowel) >= 3

def letterAppearsTwiceInARow(s: String): Boolean = s.matches(".*([a-z])\\1.*")

def containsForbiddenString(s: String): Boolean = (s contains "ab") || (s contains "cd") || (s contains "pq") || (s contains "xy")

def isNice(s: String): Boolean = containsAtLeastThreeVowels(s) && letterAppearsTwiceInARow(s) && !containsForbiddenString(s)

def day05Part1: Int = strings.count(isNice)

def containsPairTwice(s: String): Boolean = s.matches(".*([a-z][a-z]).*\\1.*")

def containsRepeatedLetterWithLetterBetween(s: String): Boolean = s.matches(".*([a-z]).\\1.*")

def isNice2(s: String): Boolean = containsPairTwice(s) && containsRepeatedLetterWithLetterBetween(s)

def day05Part2: Int= strings.count(isNice2)
