package v2

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scalaz.NonEmptyList
import scalaz.NonEmptyList._

sealed trait History extends Iterable[String]{

  def + (x: String): History
  def size: Int

  def head: String
  def apply(i: Int): String
  def get(i: Int): Option[String] = if (i < 0 || i >= size) None else Some(apply(i))
}

object History {

  @inline def log2(i: Int) = 31 - Integer.numberOfLeadingZeros(i)
  @inline def isPowerOf2(i: Int) = i == (1 << log2(i))

  @tailrec final def findInPast(i: Int, past: List[FullHistory]): String = past match {
    case Nil     => throw new IndexOutOfBoundsException
    case h :: hs => if (i < h.size) h(i) else findInPast(i - h.size, past.tail)
  }

  implicit def nel2list[A](nel: NonEmptyList[A]): List[A] = nel.list

  case object EmptyHistory extends EvenHistory {
    override def +(x: String): SingleHistory = SingleHistory(x)

    override def size: Int = 0
    override def apply(i: Int) = ???
    override def head: String = ???

    override def iterator: Iterator[String] = Iterator.empty
  }

  sealed trait NonEmptyHistory extends History {
    def past: List[FullHistory]
    override def apply(i: Int) = if (i==0) head else History.findInPast(i-1, past)
    def size: Int

    def head: String
    def iterator = new HistoryIterator(this)

    def calcsize: Int = 1 + past.map(_.calcsize).sum
  }

  sealed trait FullHistory extends NonEmptyHistory

  sealed trait EvenHistory extends History {
    override def +(x: String): OddHistory
  }

  sealed trait OddHistory extends NonEmptyHistory {
    override def +(x: String): EvenHistory
  }

  case class SingleHistory(override val head: String) extends FullHistory with OddHistory {

    override def size = 1
    def past = Nil

    override def +(x: String): EvenHistory = FullXHistory(2, x, NonEmptyList(this))
  }

  case class FullXHistory(override val size: Int, override val head: String, nePast: NonEmptyList[FullHistory])
    extends FullHistory with EvenHistory {
//    println(s"Full size: $size $calcsize, head: $head, past-length: ${past.size}")
    override def +(x: String): OddPartHistory = OddPartHistory(size + 1, x, NonEmptyList(this))
    def past = nePast
  }

  case class OddPartHistory(override val size: Int, override val head: String, nePast: NonEmptyList[FullHistory])
    extends OddHistory {
//    println(s"Part size: $size, head: $head, past-length: ${past.size}, calcsize: $calcsize")

//    println(s"log2(size+1): ${log2(size+1)}, 1 << log2(size+1): ${1 << log2(size+1)}")

    def almostFull = isPowerOf2(size+1)
    def past = nePast

    override def +(x: String): EvenHistory =
      if (almostFull)
        FullXHistory(size + 1, x, SingleHistory(head) <:: nePast)
      else
        EvenPartHistory(size + 1, x, SingleHistory(head) <:: nePast)
  }

  case class EvenPartHistory(override val size: Int, override val head: String, nePast: NonEmptyList[FullHistory])
    extends NonEmptyHistory with EvenHistory {
//    println(s"Part size: $size, head: $head, past-length: ${past.size}, calcsize: $calcsize")
    def past = nePast

    override def +(x: String): OddPartHistory = OddPartHistory(size + 1, x, findRun(past.tail, NonEmptyList(past.head)))

    @tailrec final
    def findRun(past: List[FullHistory], run: NonEmptyList[FullHistory], s: Int = 2): NonEmptyList[FullHistory] = {
      require(past.nonEmpty, "past mu")
      if (past.head.size != s)
        nel(FullXHistory(1 << run.size, head, run.reverse), past)
      else
        findRun(past.tail, past.head <:: run, s * 2)
    }
  }

  object HistoryListIterator {
    def apply(list: List[NonEmptyHistory]) = list match {
      case Nil => Iterator.empty
      case h :: hs => new HistoryListIterator(nel(h, hs))
    }
  }

  class HistoryListIterator[V, T](list: NonEmptyList[NonEmptyHistory]) extends AbstractIterator[String] {
    var curr: Iterator[String] = list.head.iterator
    var past = list.tail

    override def hasNext: Boolean = curr.hasNext || past.nonEmpty


    override def next(): String = {
      if (!curr.hasNext) curr = past match {
        case Nil => Iterator.empty
        case h :: hs =>
          past = hs
          h.iterator
      }
      curr.next()
    }
  }

  class HistoryIterator[V, T](h: NonEmptyHistory) extends AbstractIterator[String] {
    var curr: Iterator[String] = this

    override def hasNext: Boolean = (curr eq this) || curr.hasNext

    override def next(): String = if (curr eq this) {
      curr = HistoryListIterator(h.past)
      h.head
    } else {
      curr.next()
    }
  }
}

