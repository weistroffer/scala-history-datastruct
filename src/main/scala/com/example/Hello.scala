package com.example

import v2._

import scala.util.Random

object Hello {
  def main(args: Array[String]): Unit = {
    println("Hello, world!")


//    var h: History = EmptyHistory
//    for (i <- m to 0 by -1) {
//      println(s"$i: ...")
//      h += s"s$i"
//      println(s"${h.getClass}: $h")
//    }
//
//    for (i <- 0 to m) {
//      println(s"$i: ${h(i)}")
//    }

  }

  val m = 20000
  val n = 20000

  val us = Range(0,n).map{_ => sampleUniform()}.toArray
  val ls = Range(0,n).map{_ => sampleLongTail()}.toArray
  val ss = Range(0,n).map{_ => sampleShortTail()}.toArray


  timeHistory.prettyPrint("History")
  timeList.prettyPrint("List")
  timeVector.prettyPrint("Vector")
  timeHistory.prettyPrint("History")

  case class Timing(build: Long, uniform: Long, longTail: Long, shortTail: Long, checksum: Int) {
    def prettyPrint(title: String) = {
      println(f"$title%9s build: ${build/1000000}%5d  uniform: ${uniform/1000000}%5d  longTail: ${longTail/1000000}%5d  shortTail: ${shortTail/1000000}%5d  checksum: $checksum%3d")
    }
  }

  def sampleUniform(): Int = Random.nextInt(m)
  def sampleLongTail(): Int = {var i=0; while(Random.nextInt(200)>0) i+=1; i}// Math.floor(Math.exp(Math.)) - 1
  def sampleShortTail(): Int = 0//{var i=0; while(Random.nextInt(30)>0) i+=1; i}

  def time[R](block: => R): Long = {
    val t0 = System.nanoTime()
    val result = block    // call-by-name
    val t1 = System.nanoTime()
    t1- t0
  }

  def timeHistory: Timing = {
    var data: History = History.EmptyHistory
    val tb = time { for (i <- 1 to m) data += s"s$i" }
    var checksum = 0
    val tu = time { for (i <- 0 until n) checksum ^= data(us(i)).length }
    val tl = time { for (i <- 0 until n) checksum ^= data(ls(i)).length }
    val ts = time { for (i <- 0 until n) checksum ^= data(ss(i)).length }
    Timing(tb, tu,tl,ts,checksum)
  }

  def timeList: Timing = {
    var data: List[String] = Nil
    val tb = time { for (i <- 1 to m) data = s"s$i" :: data}
    var checksum = 0
    val tu = time { for (i <- 0 until n) checksum ^= data(us(i)).length }
    val tl = time { for (i <- 0 until n) checksum ^= data(ls(i)).length }
    val ts = time { for (i <- 0 until n) checksum ^= data(ss(i)).length }
    Timing(tb, tu,tl,ts,checksum)
  }

  def timeVector: Timing = {
    var data: Vector[String] = Vector.empty
    val tb = time { for (i <- 1 to m) data = s"s$i" +: data}
    var checksum = 0
    val tu = time { for (i <- 0 until n) checksum ^= data(us(i)).length }
    val tl = time { for (i <- 0 until n) checksum ^= data(ls(i)).length }
    val ts = time { for (i <- 0 until n) checksum ^= data(ss(i)).length }
    Timing(tb, tu,tl,ts,checksum)
  }


}
