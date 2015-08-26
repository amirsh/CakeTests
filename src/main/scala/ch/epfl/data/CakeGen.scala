package ch.epfl.data

import java.io.{File, PrintWriter}

import ch.epfl.data.sc.pardis.utils.document._

object CakeGen extends App {
//  val (numOps, numDefs, numCakes) = (3, 2, 4)
  val (numOps, numDefs, numCakes) = (30, 20, 40)

  val oldCode = Code("Old", numOps, numDefs, numCakes, implicits = true)
  val oldCodeSelfless = Code("OldSelfless", numOps, numDefs, numCakes, implicits = true, selfTypes = false)
  val newCode = Code("New", numOps, numDefs, numCakes, implicits = false)
  val oneCode = Code("One", numOps, numDefs, numCakes, implicits = false, oneCake = true)

  println(oldCode)
  
  val folder = new File("generated")
  def output(text: String, fname: String) = {
    val outputFile = new PrintWriter(new File(folder, fname))
    outputFile.println(text)
    outputFile.close()
  }
  output(oldCode.toString, "CakeOld.scala")
  output(oldCodeSelfless.toString, "CakeOldSelfless.scala")
  output(newCode.toString, "CakeNew.scala")
  output(oneCode.toString, "CakeOne.scala")
  
  println("Done.")
}

object Code {
  def apply(name: String,
            numOps: Int,
            numDefs: Int,
            numCakes: Int,
            implicits: Boolean,
            oneCake: Boolean = false,
            selfTypes: Boolean = true) = {
    val db = new DocBuilder()
    
    val cake = 1 to numOps map {"Ops"+_} mkString " with "
    
    db.bracesAfter(doc"object $name") {
      
      db.bracesAfter(doc"trait Base") {
        db +=\\ doc"class Rep[+T]"
        db +=\\ doc"def lift[T](t: T): Rep[T] = new Rep[T]"
      }
      
      for (i <- 1 to numOps) {
        val tname = s"T$i"
        db.bracesAfter(doc"trait Ops$i extends Base") {
          if (selfTypes) db +=\\ doc"this: $cake => "
          db +=\\ doc"class $tname"
          db.bracesAfter(doc"implicit class Rep$i(self: Rep[$tname])") {
            for (j <- 1 to numDefs)
              db +=\\ doc"def m$j: Rep[Int] = ${tname}d$j(self)"
          }
          for (j <- 1 to numDefs)
            db +=\\ doc"def ${tname}d$j(x: Rep[$tname]): Rep[Int] = lift(42)"
        }
      }
      
      if (oneCake)
        db +=\\ doc"object Cake extends $cake"
      
      def mkCalls = for (i <- 1 to numOps; j <- 1 to numDefs) {
        def TRep = doc"lift(new T$i)"
        if (implicits) db +=\\ doc"$TRep.m$j"
        else db +=\\ doc"T${i}d$j($TRep)"
      }
      
      for (c <- 1 to numCakes)
        if (oneCake) db.bracesAfter(doc"object Use$c") {
          db +=\\ doc"import Cake._"
          mkCalls
        } else db.bracesAfter(doc"class Cake$c extends " + cake) {
          mkCalls
        }
    
    }
    
    db.toDoc
  }
}














