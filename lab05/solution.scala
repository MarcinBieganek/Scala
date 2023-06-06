// Marcin Bieganek

package plugins {
    abstract class Pluginable {
        def plugin(text: String) = text
    }

    trait Reverting extends Pluginable {
        abstract override def plugin(text: String) = super.plugin(text.reverse)
    }

    trait LowerCasing extends Pluginable {
        abstract override def plugin(text: String) = super.plugin(text.toLowerCase())
    }

    trait SingleSpacing extends Pluginable {
        abstract override def plugin(text: String) = super.plugin(text.replaceAll(" +", " "))
    }

    trait NoSpacing extends Pluginable {
        abstract override def plugin(text: String) = super.plugin(text.replaceAll(" +", ""))
    }

    trait DuplicateRemoval extends Pluginable {
        abstract override def plugin(text: String) = super.plugin(for {
            c <- text
            if text.filter(_ == c).length() == 1    
        } yield c)
    }

    trait Rotating extends Pluginable {
        abstract override def plugin(text: String) = super.plugin(text.takeRight(1) + text.dropRight(1))
    }

    trait Doubling extends Pluginable {
        abstract override def plugin(text: String) = super.plugin(text.zipWithIndex.map{case (c, index) => if (index % 2 != 0) c.toString()+c.toString() else c}.mkString)
    }

    trait Shortening extends Pluginable {
        abstract override def plugin(text: String) = super.plugin((for {
            (c, index) <- text.zipWithIndex
            if index % 2 == 0
        } yield c).mkString)
    }

    object Actions {
        val actionA: Pluginable = new Pluginable() with Shortening with Doubling with SingleSpacing

        val actionB: Pluginable = new Pluginable() with Doubling with Shortening with NoSpacing

        val actionC: Pluginable = new Pluginable() with Doubling with LowerCasing

        val actionD: Pluginable = new Pluginable() with Rotating with DuplicateRemoval

        val actionE: Pluginable = new Pluginable() with Reverting with Doubling with Shortening with NoSpacing

        val actionF: Pluginable = {
            class RotatingNTimes extends Rotating {
                override def plugin(text: String) = repeated(super.plugin, 5)(text)

                private def repeated[A, B](f: (A) => A, n: Int): (A) => A = {
                    if (n == 1) (x: A) => f(x)
                    else (x: A) => f(repeated(f, n-1)(x))
                } 
            }
            new RotatingNTimes()
        }

        val actionG: Pluginable = {
            class ActionAB extends Pluginable {
                override def plugin(text: String) =  actionB.plugin(actionA.plugin(text))
            }
            new ActionAB()
        }
    }
}

object MyApplication {
    def main(args: Array[String]) {
        import plugins._
        
        println("========== Reverting test ==========")
        class ClassReverting extends Reverting {}

        val cr = new ClassReverting()
        val text1 = "abcd"

        println("before: " + text1)
        println("after: " + cr.plugin(text1))
        println("====================================")

        println("========== LowerCasing test ========")
        class ClassLowerCasing extends LowerCasing {}

        val cl = new ClassLowerCasing()
        val text2 = "ABCD"

        println("before: " + text2)
        println("after: " + cl.plugin(text2))
        println("====================================")

        println("========== SingleSpacing test ========")
        class ClassSingleSpacing extends SingleSpacing {}

        val cs = new ClassSingleSpacing()
        val text3 = "ala ma   kota"

        println("before: " + text3)
        println("after: " + cs.plugin(text3))
        println("=================================")

        println("========== NoSpacing test ========")
        class ClassNoSpacing extends NoSpacing {}

        val cn = new ClassNoSpacing()
        val text4 = "ala ma   kota"

        println("before: " + text4)
        println("after: " + cn.plugin(text4))
        println("=================================")

        println("========== DuplicateRemoval test ========")
        class ClassDuplicateRemoval extends DuplicateRemoval {}

        val cd = new ClassDuplicateRemoval()
        val text5 = "alzaa  cda"

        println("before: " + text5)
        println("after: " + cd.plugin(text5))
        println("=================================")

        println("========== Rotating test ========")
        class ClassRotating extends Rotating {}

        val cr2 = new ClassRotating()
        val text6 = "abc"

        println("before: " + text6)
        println("after: " + cr2.plugin(text6))
        println("=================================")

        println("========== Doubling test ========")
        class ClassDoubling extends Doubling {}

        val cd2 = new ClassDoubling()
        val text7 = "abcd"

        println("before: " + text7)
        println("after: " + cd2.plugin(text7))
        println("=================================")

        println("========== Shortening test ========")
        class ClassShortening extends Shortening {}

        val cs2 = new ClassShortening()
        val text8 = "ab cd"

        println("before: " + text8)
        println("after: " + cs2.plugin(text8))
        println("=================================")

        println("========== actionA test ========")
        
        val text9 = "abc   d"
        //          "abc d"     <- SingleSpacing
        //          "abbc  d"   <- Doubling
        //          "ab d"      <- Shortening

        println("before: " + text9)
        println("after: " + Actions.actionA.plugin(text9))
        println("=================================")

        println("========== actionB test ========")
        
        val text10 = "abc   d"
        //           "abcd"     <- NoSpacing
        //           "ac"       <- Shortening
        //           "acc"      <- Doubling

        println("before: " + text10)
        println("after: " + Actions.actionB.plugin(text10))
        println("=================================")

        println("========== actionC test ========")
        
        val text11 = "ABCD"
        //           "abcd"        <- LowerCasing
        //           "abbcdd"      <- Doubling

        println("before: " + text11)
        println("after: " + Actions.actionC.plugin(text11))
        println("=================================")

        println("========== actionD test ========")
        
        val text12 = "1a1b  c"
        //           "abc"     <- DuplicateRemoval
        //           "cba"     <- Rotating

        println("before: " + text12)
        println("after: " + Actions.actionD.plugin(text12))
        println("=================================")

        println("========== actionE test ========")
        
        val text13 = "ab  c d"
        //           "abcd"      <- NoSpacing
        //           "ac"        <- Shortening
        //           "acc"       <- Doubling
        //           "cca"       <- Reverting

        println("before: " + text13)
        println("after: " + Actions.actionE.plugin(text13))
        println("=================================")

        println("========== actionF test ========")
        
        val text14 = "abcd12345"
        //           "5abcd1234"      
        //           "45abcd123"       
        //           "345abcd12"       
        //           "2345abcd1"      
        //           "12345abcd"       

        println("before: " + text14)
        println("after: " + Actions.actionF.plugin(text14))
        println("=================================")

        println("========== actionG test ========")
        
        val text15 = "abc   d"
        //          "ab d"      <- actionA 
        //          "add"       <- actionB

        println("before: " + text15)
        println("after: " + Actions.actionG.plugin(text15))
        println("=================================")
    }
}