package stagedparsec

import scala.virtualization.lms.common._
import lms._
import lms.util._

/**
 * An implementation of staged parser combinators
 * based on a previous implementation in
 * https://github.com/manojo/experiments/
 */

trait StagedParsers
    extends ParseResultOps
    with OptionOps
    with ReaderOps
    with MyTupleOps
    with IfThenElse {

  abstract class Parser[+T: Manifest]
      extends (Rep[Input] => Rep[ParseResult[T]]) {

    /**
     * The flatMap operation
     */
    private def flatMap[U: Manifest](f: Rep[T] => Parser[U]) = Parser[U] { input =>
      val tmp = this(input)
      if (tmp.isEmpty) Failure[U](input)
      else {
        val x = f(tmp.get).apply(tmp.next)
        if (x.isEmpty) Failure[U](input) else x
      }
    }

    def >>[U: Manifest](f: Rep[T] => Parser[U]) = flatMap(f)

    /**
     * The concat operation
     * implementing with `flatMap` produces worse code
     * TODO: check the reason
     */
    def ~[U: Manifest](that: Parser[U]) = Parser[(T, U)] { input =>
      val x = this(input)
      if (x.isEmpty) Failure[(T, U)](input)
      else {
        val y = that(x.next)
        if (y.isEmpty) Failure[(T, U)](input)
        else Success(make_tuple2(x.get, y.get), y.next)
      }
    }

    /**
     * get right hand side result
     */
    def ~>[U: Manifest](that: => Parser[U]) = Parser[U] { input =>
      val x = this(input)
      if (x.isEmpty) Failure[U](input) else that(x.next)
    }

    /**
     * get left hand side result
     */
    def <~[U: Manifest](that: => Parser[U]) = Parser[T] { input =>
      val x = this(input)

      if (x.isEmpty) x
      else {
        val y = that(x.next)
        if (y.isEmpty) Failure[T](input) else Success(x.get, y.next)
      }
    }

    /**
     * The map operation
     */
    def map[U: Manifest](f: Rep[T] => Rep[U]) = Parser[U] { input =>
      this(input) map f
    }

  }

  /**
   * a 'conditional' parser
   * lifts conditional expressions to parser level
   */
  def __ifThenElse[A: Manifest](
    cond: Rep[Boolean],
    thenp: => Parser[A],
    elsep: => Parser[A]
  ): Parser[A] = Parser[A] { input => if (cond) thenp(input) else elsep(input) }

  /**
   * companion object for apply function
   */
  object Parser {
    def apply[T: Manifest](f: Rep[Input] => Rep[ParseResult[T]]) = new Parser[T] {
      def apply(in: Rep[Input]) = f(in)
    }

    /**
     * run a parser, and return an `Option`
     */
    def phrase[T: Manifest](p: => Parser[T], in: Rep[Input]): Rep[Option[T]] = {
      val presult = p(in)
      val res = if (presult.isEmpty) none[T]() else Some(presult.get)
      res
    }
  }
}

trait StagedParsersExp
    extends StagedParsers
    with ParseResultOpsExp
    with OptionOpsExp
    with MyTupleOpsExp
    with IfThenElseExpOpt
    with BooleanOpsExpOpt
    with EqualExpOpt


trait ScalaGenStagedParsers
    extends ScalaGenParseResultOps
    with ScalaGenOptionOps
    with ScalaGenMyTupleOps
    with ScalaGenIfThenElse
    with ScalaGenBooleanOps
    with ScalaGenEqual {
  val IR: StagedParsersExp
}
