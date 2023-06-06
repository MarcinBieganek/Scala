// Marcin Bieganek

object Utils {
    def isSorted(as: List[Int], ordering: (Int, Int) => Boolean): Boolean = {
        if (as.isEmpty) true
        else {
            var prev = as.head
            var ls = as.tail
            while (!ls.isEmpty) {
                if (!ordering(prev, ls.head)) return false
                else {
                    prev = ls.head
                    ls = ls.tail
                }
            }
            true
        }
    }

    def isAscSorted(as: List[Int]) = isSorted(as, (x, y) => (x < y))

    def isDescSorted(as: List[Int]) = isSorted(as, (x, y) => (x > y))

    def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = {
        if (l.isEmpty) z
        else foldLeft(l.tail, f(z, l.head))(f)
    }

    def sum(l: List[Int]) = foldLeft(l, 0)( (x, y) => x+y )

    def length[A](l: List[A]) = foldLeft(l, 0)( (x, y) => x + 1 )

    def compose[A, B, C](f: (B) => C, g: (A) => B) = (x: A) => f(g(x))

    def repeated[A, B](f: (A) => A, n: Int): (A) => A = {
        if (n < 1) throw new Exception()
        else if (n == 1) (x: A) => f(x)
        else (x: A) => f(repeated(f, n-1)(x))
    }

    def curry[A, B, C](f: (A, B) => C) = (x: A) => ((y: B) => f(x, y))

    def uncurry[A, B, C](f: (A) => ((B) => C)) = (x: A, y: B) => f(x)(y) 

    def unSafe[T](ex: Exception)(code: => T) = {
        try {
            code
        } catch {
            case t: Throwable => {
                //t.printStackTrace()
                println("unSafe catched following exception: " + t)
                throw ex
            }
        }
    }
}

case class MyException(message: String) extends Exception(message)
case class MyDifferentException(message: String) extends Exception(message)

object MyApplication {
    def main(args: Array[String]) {
        println("====== isSorted testing =======")
        
        var l_sorted = List(1, 2, 3, 4, 5)
        println(l_sorted)
        println(Utils.isSorted(l_sorted, (x, y) => (x < y)))

        var l_notsorted = List(1, 2, 3, 10, 0)
        println(l_notsorted)
        println(Utils.isSorted(l_notsorted, (x, y) => (x < y)))

        println("==============================")
        
        println("====== isAscSorted testing =======")
        
        l_sorted = List(1, 2, 3, 4, 5)
        println(l_sorted)
        println(Utils.isAscSorted(l_sorted))

        l_notsorted = List(100, 2, 3, 10, 0)
        println(l_notsorted)
        println(Utils.isAscSorted(l_notsorted))

        println("==============================")

        println("====== isDescSorted testing =======")
        
        l_sorted = List(5, 4, 3, 2, 1)
        println(l_sorted)
        println(Utils.isDescSorted(l_sorted))

        l_notsorted = List(100, 2, 3, 10, 1000)
        println(l_notsorted)
        println(Utils.isDescSorted(l_notsorted))

        println("==============================")

        println("====== foldLeft testing =======")
        
        l_sorted = List(5, 4, 3, 2, 1)
        println(l_sorted)
        println("sum: " + Utils.foldLeft(l_sorted, 0)( (x, y) => x+y ))

        println("==============================")

        println("====== sum testing =======")
        
        l_sorted = List(5, 4, 3, 2, 1)
        println(l_sorted)
        println("sum: " + Utils.sum(l_sorted))

        println("==============================")

        println("====== length testing =======")
        
        l_sorted = List(5, 4, 3, 2, 1)
        println(l_sorted)
        println("length: " + Utils.length(l_sorted))

        println("==============================")

        println("====== compose testing =======")
        
        l_sorted = List(5, 4, 3, 2, 1)
        println(l_sorted)
        println("length > 10 ?: " + Utils.compose( ((x: Int) => x > 10), Utils.length )(l_sorted))

        println("==============================")

        println("====== repeated testing =======")
        
        println("repeated +2 3 times: " + Utils.repeated( (x: Int) => x+2, 3 )(0))

        println("==============================")

        println("====== curry testing =======")

        def add(a: Int, b: Int) = a + b
        
        println("add(1, 2): " + add(1, 2))
        println("curry(add)(1)(2): " + Utils.curry(add)(1)(2))

        println("==============================")

        println("====== uncurry testing =======")

        def curryadd = Utils.curry(add)
        
        println("curryadd(1)(2): " + curryadd(1)(2))
        println("uncurry(curryadd)(1, 2): " + Utils.uncurry(curryadd)(1, 2))

        println("==============================")

        println("====== unSafe testing =======")


        // Uncomment one of following test examples:
        /*
        Utils.unSafe(MyException("Could not run command")) {
            var divByZero = 10 / 0 
        }
        */

        /*
        Utils.unSafe(MyDifferentException("Error while building map")) {
            throw new MyException("My Exception used in unSafe second test")
        }
        */

        println("==============================")

    }
}