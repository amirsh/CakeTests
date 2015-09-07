package ch.epfl.data

import Common._

object VerboseAnalyse extends App {
  import scala.io.Source._
  
  val PhaseTime = raw"\[(.*) in (\d+)ms\]".r
  
  args.headOption match {
    case Some(fname) =>
      val phases = (fromFile(fname).getLines flatMap {
        case PhaseTime(name, time) => Some(name, time.toInt)
        case _ => None
      }).toSeq sortBy (-_._2)
      println("My Total: "+(phases map {
        case("total", _) => 0
        case(_, time) => time
      }).sum)
      phases take 10 foreach { case(name, time) => println(s"${name take 32}: $time") }
    case None =>
      System.err.println("Expected file name")
      System.exit(-1)
  }
  
  
  
}

/*
With:
  scalac ../bench/Bench-true-true.scala -verbose -d ../bench/Bench-true-true-bin 2> out

My Total: 28411
total: 27499
typer: 17871
jvm: 1961
erasure: 1875
Generate ICode from the AST: 838
mixin: 690
parser: 572
uncurry: 481
explicitouter: 472
refchecks: 465

Looks like the most expensive phase (typer) is only 60% if the time
The rest seems pretty evenly distributed...

BUT with:
  scalac ../bench/Bench-false-false.scala -verbose -d ../bench/Bench-false-false-bin 2> out

My Total: 33905
total: 33009
jvm: 15023
mixin: 6365
typer: 4598
erasure: 1837
Generate ICode from the AST: 1210
parser: 662
refchecks: 529
explicitouter: 494
specialize: 372

'JVM' now dominates! What is it?!
Could it be due to the difference in file size? (776Kb versus 509Kb) -- seems unlikely

*/







