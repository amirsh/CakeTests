package ch.epfl.data

import java.io.File

import Common._

import scala.collection.mutable.ArrayBuffer

object Benchmark extends App {
  import sys.process._
  
  case class Result(ops: Int, cakes: Int, impl: Option[Boolean], one: Boolean, mixin: Long, erasure: Long, typer: Long, jvm: Long, total: Long, user: Double) {
    def implStr = impl match {
      case Some(true)  => "Inferred"
      case Some(false) => "Provided"
      case None        => "None"
    }
    def toCsv = s"$ops, $cakes, $implStr, $one, $mixin, $erasure, $typer, $jvm, $total, $user"
  }
  object Result {
    def csvHeaders = "Ops, Cakes, Impl, One, Mixin, Erasure, Typer, JVM, Total, User"
  }
  
  case class Faulty(ops: Int, cakes: Int, impl: Option[Boolean], one: Boolean, err: Throwable)
  
  val folder = new File("../bench")
  
  // ops: 85, cakes: 1, defs: 30:
  // => "Could not write class Bench$Use1$ because it exceeds JVM code size limits. Method Bench$Ops1$T1's code too large!"
  
//  val opsRange = 0 to 80 by 10
//  val cakesRange = 1 to 31 by 10
//  val defs = 25 // 30
  
//  // Debugging:
//  val opsRange = 5 to 5 by 5
//  val cakesRange = 1 to 1 by 5
//  val defs = 25
  
  // Debugging:
  val opsRange = 80 to 80 by 5
  val cakesRange = 31 to 31 by 5
  val defs = 25 // 30
  
  val ProcessTimes = raw"\s*(.*)\sreal\s*(.*)\suser\s*(.*)\ssys".r
  
  def phaseTime(name: String) = raw"\[$name in (\d+)ms\]".r
  
  val MixinTime = phaseTime("mixin")
  val ErasureTime = phaseTime("erasure")
  val TyperTime = phaseTime("typer")
  val JVMTime = phaseTime("jvm")
  val TotalTime = phaseTime("total")
  
  val totalComp = opsRange.size * cakesRange.size * 2 * 3
  var currentComp = 0
  
  val errLines, outLines = new ArrayBuffer[String](1000)
  val faults = ArrayBuffer[Faulty]()
  
  val ress = (for {
    impl <- Seq(Some(true), Some(false), None)
    one <- Seq(true, false)
    cakes <- cakesRange
    ops <- opsRange
  } yield try {
      currentComp += 1
      println(s"$currentComp out of $totalComp")
      
      errLines.clear()
      outLines.clear()
      
      val name = s"Bench-$impl-$one"
      output(Code("Bench", ops, defs, cakes, implicits = impl, oneCake = one).toString, folder, name+".scala")
      
      s"mkdir -p ${folder}/$name-bin".! // -p: ignore if dir already exists
      
      var mixin, erasure, typer, total, jvm = Option.empty[Long]
      var usr = Option.empty[Double]
      
      val cmd = s"time scalac ${folder}/$name.scala -verbose -d ${folder}/$name-bin"
      println(cmd)
      cmd lineStream ProcessLogger(
      
      ln => outLines += ln, {
        case MixinTime(t) => mixin = Some(t.toLong)
        case ErasureTime(t) => erasure = Some(t.toLong)
        case TyperTime(t) => typer = Some(t.toLong)
        case JVMTime(t) => jvm = Some(t.toLong)
        case TotalTime(t) => total = Some(t.toLong)
        case ProcessTimes(r, t, _) =>
          println(s"Total: Scala said $total; time.real said $r")
          usr = Some(t.toDouble)
        case ln => errLines += ln
      })
      
      val res = Result(
        ops, cakes, impl, one,
        mixin.get, erasure.get, typer.get, jvm.get, total.get,
        usr.get
      )
      
      println(res)
      
      Some(res)
  } catch {
      case e: Throwable =>
        System.err.println("Caught "+e)
        System.err.println("\nError lines:")
        errLines foreach System.err.println
        System.err.println("\nOutput lines:")
        outLines foreach System.err.println
        faults += Faulty(ops, cakes, impl, one, e)
        None
  }).flatten
  
  printToFile(new File(folder, "res.csv")) { p =>
    p.println(Result.csvHeaders)
    ress map (_.toCsv) foreach p.println
  }
  
  println("Done.")
  println("Faulty configurations:")
  faults foreach println
  
  
}








