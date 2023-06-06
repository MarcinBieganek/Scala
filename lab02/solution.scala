// Marcin Bieganek

package lab2.numbers {
    class Rational(n : Int, d : Int) {
        require(d != 0, "Denominator cannot equal 0 (zero)!")
        private def gcd(a : Int , b : Int) : Int = if (b == 0) a else gcd (b, a % b)
        private val g = gcd (n.abs, d.abs)

        val num = n / g
        val den = d / g

        override def toString = {
            if (num == 0) "0"
            else {
                val integ = if ((num / den) != 0) (num / den) + " " else ""
                val fract = if ((num % den) != 0) (num % den) + "/" + den else ""
                integ + fract
            }
        }
        def toDouble = num.toDouble / den.toDouble

        def +(other: Rational): Rational = new Rational(num * other.den + den * other.num, den * other.den)
        def -(other: Rational): Rational = new Rational(num * other.den - den * other.num, den * other.den)
        def *(other: Rational): Rational = new Rational(num * other.num, den * other.den)
        def /(other: Rational): Rational = new Rational(num * other.den, den * other.num)   
    }

    object Rational {
        def zero(): Rational  = new Rational (0, 1)
        def one(): Rational = new Rational (1, 1)
        def apply(n: Int): Rational = new Rational (n, 1)
    }
}

package lab2.figures {
    class Point(val x: lab2.numbers.Rational, val y: lab2.numbers.Rational) {
        override def toString = "(" + x + ", " + y + ")"

        def dist(other: Point): Double = {
            import math.{sqrt, pow}
            math.sqrt(math.pow((other.x - x).toDouble, 2) + math.pow((other.y - y).toDouble, 2))
        }
    }

    class Figure() {
        def area: Double = 0.0
        val description = "Figure"
    }

    class Triangle(val p1: Point, val p2: Point, val p3: Point) extends Figure() {
        override def area: Double = {
            import math.sqrt
            val b1 = p2.dist(p1)
            val b2 = p2.dist(p3)
            val b3 = p1.dist(p3)
            val p = (b1 + b2 + b3) / 2
            math.sqrt(p * (p - b1) * (p - b2) * (p - b3))
        }
        override val description = "Triangle"
    }

    class Rectangle(p1: Point, p2: Point, p3: Point, p4: Point) extends Figure() { // vertices must be in order (e.g. ABCD or ACDB)
        override def area: Double = {
            val b1 = p1.dist(p2)
            val b2 = p2.dist(p3)
            val b3 = p3.dist(p4)
            val b4 = p4.dist(p1)
            if (b1 == b2) b1 * b3 else b1 * b2
        }
        override val description = "Rectangle"
    }

    object Rectangle {
        def onebytwo(): Rectangle = {
            val p1 = new Point(lab2.numbers.Rational.zero, lab2.numbers.Rational.zero)
            val p2 = new Point(lab2.numbers.Rational.zero, lab2.numbers.Rational.one)
            val p3 = new Point(lab2.numbers.Rational.apply(2), lab2.numbers.Rational.one)
            val p4 = new Point(lab2.numbers.Rational.apply(2), lab2.numbers.Rational.zero)
            new Rectangle(p1, p2, p3, p4)
        }
        def oneverticeandsize(p1: Point, height: Int, width: Int) = {
            val p2 = new Point(p1.x + lab2.numbers.Rational.apply(width), p1.y)
            val p3 = new Point(p1.x + lab2.numbers.Rational.apply(width), p1.y + lab2.numbers.Rational.apply(height))
            val p4 = new Point(p1.x, p1.y + lab2.numbers.Rational.apply(height))
            new Rectangle(p1, p2, p3, p4)
        }
    }

    class Square(p1: Point, p2: Point, p3: Point, p4: Point) extends Figure() { // vertices must be in order (e.g. ABCD or ACDB)
        override def area: Double = {
            val b1 = p1.dist(p2)
            b1 * b1
        }
        override val description = "Square"
    }

    object Square {
        def onebyone(): Square = {
            val p1 = new Point(lab2.numbers.Rational.zero, lab2.numbers.Rational.zero)
            val p2 = new Point(lab2.numbers.Rational.zero, lab2.numbers.Rational.one)
            val p3 = new Point(lab2.numbers.Rational.one, lab2.numbers.Rational.one)
            val p4 = new Point(lab2.numbers.Rational.one, lab2.numbers.Rational.zero)
            new Square(p1, p2, p3, p4)
        }
        def oneverticeandsize(p1: Point, size: Int) = {
            val p2 = new Point(p1.x + lab2.numbers.Rational.apply(size), p1.y)
            val p3 = new Point(p1.x + lab2.numbers.Rational.apply(size), p1.y + lab2.numbers.Rational.apply(size))
            val p4 = new Point(p1.x, p1.y + lab2.numbers.Rational.apply(size))
            new Square(p1, p2, p3, p4)
        }
    }

    object Figures_singleton{
        def areaSum(figures: List[Figure]): Double = (for (fig <- figures) yield fig.area).sum
        def printAll(figures: List[Figure]): Unit = for (fig <- figures) println(fig.description)
    }
}

object MyApplication {
    def main(args: Array[String]) {
        println("\n==== Rational tests ====\n")

        import lab2.numbers._
        
        val r1 = new Rational(1, 2)
        val r2 = new Rational(1, 3)
        
        println(r1)
        println(r2)
        println(r1 + r2)
        println(r1 - r2)
        println(r1 * r2)
        println(r1 / r2)

        println(Rational.zero)
        println(Rational.one)
        println(Rational.apply(10))

        println("\n==== Figures tests =====\n")

        import lab2.figures._

        val r = new Rational(3, 2)

        val p1 = new Point(Rational.zero, Rational.zero)
        val p2 = new Point(Rational.zero, r)
        val p3 = new Point(r, Rational.zero)
        val p4 = new Point(r, r)

        val s = new Square(p1, p2, p4, p3)

        println("s area: " + s.area)
        println("s desc: " + s.description)

        val re = new Rectangle(p1, p2, p4, p3)
        println("re area: " + re.area)
        println("re desc: " + re.description)

        val s2 = Square.oneverticeandsize(p1, 10)
        println("s2 area: " + s2.area)
        val re2 = Rectangle.oneverticeandsize(p1, 5, 20)
        println("re2 area: " + re2.area)

        val li = List(s, re, s2, re2)

        println("sum area " + Figures_singleton.areaSum(li))
        println("print all")
        Figures_singleton.printAll(li)
    }
}