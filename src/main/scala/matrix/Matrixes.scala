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
    type Answer = Cost //MatrixAlgebra
    def single:Alphabet => Answer
    def mult:(Answer, Answer) => Answer
  }

  trait MatrixGrammar extends BaseParsers with MatrixSig {
    //def tabulate(name:String, inner: => Parser[Answer], alwaysValid:Boolean=false) = new Tabulate(inner,name,alwaysValid)
    //class Tabulate(in: => Parser[Answer], val name:String, val alwaysValid:Boolean=false) extends Parser[Answer]

    // Monoid?
    val simple:Parser[Answer] = ((el map single)) | ((el ~ simple) map {case (x,y) => y})
    val comp:Parser[Answer] = ((el map single) | ((el ~ comp) map {case (x,y) => println(x+" "+y); mult(single(x),y)})).aggregate(x => List(x.head))
    val chain:Parser[Answer] = (
      (el map (single))
        | ((chain ~ chain) map {case (a1, a2) => mult(a1, a2)})
    )

    //val axiom=chain //FIXME: needed?
  }

  trait MatrixAlgebra extends MatrixSig {
    //val h = minBy[Answer, Int] //FIXME
    val single = (m: Alphabet) => Cost(0, m) //0 cost if no multiplication
    val mult = (c1: Answer, c2: Answer) => Cost(c1.v + c2.v + c1.m.x*c2.m.x*c2.m.y, Matrix(c1.m.x, c2.m.y))
  }

  class MatrixTest extends MatrixGrammar with MatrixAlgebra {
    def input = Array(Matrix(1,2), Matrix(2,3), Matrix(3,4)) //define the input or the alphabet translations?
  }

  val mm = new MatrixTest()
  //val mats = Array((3,2),(2,4),(4,2),(1,2))
  val res = mm.Parser[(Int, Int)](List(_))((1, 2))
  println(res)
  //val res2 = mm.comp(Matrix(1,2))
  //println(res2)
}
