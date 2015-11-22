package v1

import scala.annotation.tailrec

/**
 * Created by pete on 11/19/15.
 */
object History {

  @inline def log2(i: Int) = 31 - Integer.numberOfLeadingZeros(i)
  @inline def isPowerOf2(i: Int) = i == (2 << log2(i))

  @tailrec final def findInPast(i: Int, past: List[FullHistory]): String = past match {
    case Nil     => throw new IndexOutOfBoundsException
    case h :: hs => if (i < h.size) h(i) else findInPast(i - h.size, past.tail)
  }


  case object EmptyHistory extends History {
    override def +(x: String): History = FullHistory(1, x, List())

    override def size: Int = 0
    //  override def capacity: Int = 0

    override def apply(i: Int) = ???
    override def head: String = ???
  }

  sealed trait NonEmptyHistory extends History {
    def past: List[FullHistory]
    override def apply(i: Int) = if (i==0) head else History.findInPast(i-1, past)

  }

  case class FullHistory(size: Int, head: String, past: List[FullHistory]) extends NonEmptyHistory {
    override def +(x: String): History = if (size > 1)
      PartHistory(size + 1, x, List(this))
    else
      FullHistory(2, x, List(this))





    //  override def capacity: Int = size
  }

  case class PartHistory(size: Int, head: String, past: List[FullHistory]) extends NonEmptyHistory {
    override def +(x: String): History = if (isPowerOf2(size+1)) FullHistory(size+1,x,FullHistory(1,head,Nil)::past) else {
      val (run, rest) = findRun(past)
      PartHistory(size + 1, x, FullHistory(1 << run.size, head, run) :: rest)
    }

    @tailrec final
    def findRun(past: List[FullHistory], run: List[FullHistory] = Nil, s: Int = 1): (List[FullHistory], List[FullHistory]) =
      if (past.head.size != s) (run, past) else findRun(past.tail, run, s*2)


    //  override def capacity: Int = 2 << History.log2(size)
  }


}

sealed trait History {

  def + (x: String): History
  def size: Int
//  def capacity: Int

  def head: String
  def apply(i: Int): String
  def get(i: Int): Option[String] = if (i < 0 || i >= size) None else Some(apply(i))
}
