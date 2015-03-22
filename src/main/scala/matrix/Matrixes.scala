package matrix

import adp._

/**
 * Created by cedricbastin on 20/03/15.
 */
object Matrixes extends App {
  case class Matrix(x:Int, y:Int)
  case class Cost(v:Int, m:Matrix)

  trait MatrixSig extends Signature {
    type Alphabet = Matrix // Input matrices as (rows, columns) we want to find best multiplication sequence
    def single:Alphabet => Answer
    def mult:(Answer, Answer) => Answer
  }

  class MatrixGrammar extends BaseParsers with MatrixSig {
    //def tabulate(name:String, inner: => Parser[Answer], alwaysValid:Boolean=false) = new Tabulate(inner,name,alwaysValid)
    //class Tabulate(in: => Parser[Answer], val name:String, val alwaysValid:Boolean=false) extends Parser[Answer]

    // Monoid?

    val chain:Parser[Answer] = (
      (el map (single))
        | ((chain ~ chain) map {case (a1, a2) => println("here"); println(a1+" "+a2); mult(a1, a2)})
    )

    val input = Array(Matrix(1,2),Matrix(2,3),Matrix(3,4)) //define the input or the alphabet translations?
    //val axiom=chain
    type Answer = Matrix//FIXME Cost //new bound and cost of multiplication (in the middle)
    //val h = minBy[Answer, Int] //FIXME
    val single = (m: Alphabet) => m//Cost(0, m) //0 cost if no multiplication
    val mult = (c1: Answer, c2: Answer) => c1//Cost(c1.v + c2.v + c1.m.x*c2.m.x*c2.m.y, Matrix(c1.m.x, c2.m.y))

  }

  trait MatrixAlgebra extends MatrixSig {
    type Answer = Matrix//FIXME Cost //new bound and cost of multiplication (in the middle)
    //val h = minBy[Answer, Int] //FIXME
    val single = (m: Alphabet) => m//Cost(0, m) //0 cost if no multiplication
    val mult = (c1: Answer, c2: Answer) => c1//Cost(c1.v + c2.v + c1.m.x*c2.m.x*c2.m.y, Matrix(c1.m.x, c2.m.y))
  }

  println("hello")
  object mm extends MatrixGrammar
  println("bye "+mm)
  //val res = mm.el((0,1) )
  //val res = mm.Parser(x => List(Matrix(x._1, x._2)))
  //println(res)
}
