
// Marcin Bieganek

val l1 = List(1, 3, -5)
val l2 = List(4, -2, -1)

//scalar product of two vectors xs and ys
def scalarUgly(xs: List[Int], ys: List[Int]) = {
    var res = 0
    var xss = xs
    var yss = ys
    while ((!xss.isEmpty) && (!yss.isEmpty)) {
        res += xss.head * yss.head
        xss = xss.tail
        yss = yss.tail
    }
    res
}
def scalar(xs: List[Int], ys: List[Int]) = {
    (for ((x, y) <- xs zip ys) yield x*y).sum
}

//quicksort algorithm
def sortUgly(xs: List[Int]): List[Int] = {
    if (xs.length <= 1) xs
    else {
        val pivot = xs(xs.length / 2)
        var left = List[Int]()
        var mid = List[Int]()
        var right = List[Int]()
        var xss = xs
        while (!xss.isEmpty) {
            if (xss.head < pivot) left = xss.head :: left
            else if (xss.head == pivot) mid = xss.head :: mid
            else right = xss.head :: right
            xss = xss.tail
        }
        sortUgly(left) ::: mid ::: sortUgly(right) 
    }
}
def sort(xs: List[Int]): List[Int] = {
    if (xs.length <= 1) xs
    else {
        val pivot = xs(xs.length / 2)
        val left = for { x <- xs
                        if (x < pivot)
                    } yield x
        val mid = for { x <- xs
                        if (x == pivot)
                    } yield x
        val right = for { x <- xs
                        if (x > pivot)
                    } yield x
        sortUgly(left) ::: mid ::: sortUgly(right) 
    }
}

//checks if n is prime
def isPrimeUgly(n: Int): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else {
        var i = 2
        while (i < n) {
            if (n % i == 0) return false
            i += 1
        }
        true
    }
}
def isPrime(n: Int): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else {
        for (i <- 2 to n-1)
            if (n % i == 0) return false
        true
    }
}