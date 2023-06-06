// Marcin Bieganek

package cards {
    abstract class Color {}

    case object Clubs extends Color {}
    case object Diamonds extends Color {}
    case object Hearts extends Color {}
    case object Spades extends Color {}

    abstract class Value {}
    abstract class Numerical extends Value {}
    abstract class Face extends Value{}

    case object Ace extends Face {}
    case object Two extends Numerical {}
    case object Three extends Numerical {}
    case object Four extends Numerical {}
    case object Five extends Numerical {}
    case object Six extends Numerical {}
    case object Seven extends Numerical {}
    case object Eight extends Numerical {}
    case object Nine extends Numerical {}
    case object Ten extends Numerical {}
    case object Jack extends Face {}
    case object Queen extends Face {}
    case object King extends Face {}

    case class Card(color: Color, value: Value)
}

package deck {
    import cards._

    class Deck(cards: List[Card]) {
        def first() = cards match {
            case Nil => Nil
            case f :: tail => f
        }
        //creates new deck without first card
        def pull() = new Deck(cards match {
            case Nil => Nil
            case _ :: tail => tail
        })
        //creates new deck with given card pushed on top
        def push(c: Card) = new Deck(c :: cards)
        // checks if deck is a standard deck
        val isStandard: Boolean = (cards.length == 52) && (cards.distinct.length == cards.length)
        //amount of duplicates of the given card in the deck
        def duplicatesOfCard(card: Card): Int = cards.count(_ == card)
        //amount of cards in the deck for the given color
        def amountOfColor(color: Color): Int = cards.count(_.color == color)
        //amount of cards in the deck for given numerical card (2, 3, 4, 5, 6, 7, 8, 9, 10)
        def amountOfNumerical(numerical: Numerical): Int = cards.count(_.value == numerical)
        //amount of all numerical cards in the deck (2, 3, 4, 5, 6, 7, 8, 9, 10)
        val amountWithNumerical: Int = cards.count(_.value match {
            case v: Numerical => true
            case _ => false
        })
        //amount of cards in the deck for the given face (Jack, Queen & King)
        def amountOfFace(face: Face): Int = cards.count(_.value == face)
        //amount of all cards in the deck with faces (Jack, Queen & King)
        val amountWithFace: Int = cards.count(_.value match {
            case v: Face => true
            case _ => false
        })

        override def toString(): String = cards.toString()
    }

    object Deck {
        //creates the standard deck with random order of cards. Check Random.shuffle1 function
        def apply() = {
            import scala.util.Random._
            var standardDeck: List[Card] = List()
            val colors = List(Clubs, Diamonds, Hearts, Spades)
            val values = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)

            for (color <- colors) {
                for (value <- values) {
                    standardDeck = Card(color, value) :: standardDeck
                }
            }
            new Deck(shuffle(standardDeck))
        }
    }
}

package games {
    import cards._
    import deck._

    class Blackjack(deck: Deck) {
        def play(n: Int): Unit = {
            var sum = 0
            var i = 0
            var deck2 = deck
            while(i < n) {
                val first = deck2.first()
                first match {
                    case c: Card => {
                        val p = points(c)
                        println((i+1) + ". " + c + " points: " + p)
                        sum += p
                    }
                }
                i += 1
                deck2 = deck2.pull()
            }
            println("points sum: " + sum)
        }

        lazy val all21: List[List[Card]] = {
            def subseq(dec: Deck): List[List[Card]] = {
                dec.first() match {
                    case Nil => Nil
                    case c: Card => {
                        val res = subseq(dec.pull())
                        List(c) :: res ++ res.map(c +: _)
                    }
                }
            }
            val allsubseq = subseq(deck)
            allsubseq.filter( (seq) => (seq.map( (card) => points(card) ).sum == 21))
        }

        def first21(): Unit = all21.head

        def points(card: Card): Int = card.value match {
            case Two => 2
            case Three => 3
            case Four => 4
            case Five => 5
            case Six => 6
            case Seven => 7
            case Eight => 8
            case Nine => 9
            case Ten | Jack | Queen | King => 10
            case Ace => 11
        }
    }

    object Blackjack {
        def apply(numOfDecks: Int) = {
            import scala.util.Random._
            var standardDeck: List[Card] = List()
            val colors = List(Clubs, Diamonds, Hearts, Spades)
            val values = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)

            for (i <- 1 to numOfDecks) {
                for (color <- colors) {
                    for (value <- values) {
                        standardDeck = Card(color, value) :: standardDeck
                    }
                }
            }
            new Blackjack(new Deck(shuffle(standardDeck)))
        }
    }
}

object MyApplication {
    def main(args: Array[String]) {

        import cards._
        import deck._
        import games._

        println("====== pull testing =======")
        
        val cards1 = new Deck( List(Card(Hearts, Queen), Card(Clubs, Two), Card(Diamonds, Ten)) )
        val pulled = cards1.pull()

        println("before: " + cards1)
        println("after:  " + pulled)

        println("===========================")

        println("====== push testing =======")
        
        val cards2 = new Deck( List(Card(Hearts, Queen), Card(Clubs, Two), Card(Diamonds, Ten)) )
        val card = Card(Spades, King)
        val pushed = cards2.push(card)

        println("before: " + cards2)
        println("before: " + card)
        println("after:  " + pushed)

        println("===========================")

        println("====== IsStandard testing =======")

        println("Deck: " + cards2)
        println("IsStandard?:  " + cards2.isStandard)

        println("=================================")

        println("====== duplicatesOfCard testing =======")

        val cards3 = new Deck( List(Card(Hearts, Queen), Card(Clubs, Two), Card(Hearts, Queen)) )
        val card2 = Card(Hearts, Queen)

        println("Deck: " + cards3)
        println("Card: " + card2)
        println("Amount of card duplicates:  " + cards3.duplicatesOfCard(card2))

        println("=======================================")

        println("====== amountOfColor testing =======")

        println("Deck: " + cards3)
        println("Color: " + Hearts)
        println("Amount of cards of color:  " + cards3.amountOfColor(Hearts))
        println("Color: " + Clubs)
        println("Amount of cards or color:  " + cards3.amountOfColor(Clubs))

        println("====================================")

        println("====== amountOfNumerical testing =======")

        println("Deck: " + cards3)
        println("Numerical: " + Two)
        println("Amount of cards:  " + cards3.amountOfNumerical(Two))
        println("Numerical: " + Ten)
        println("Amount of cards:  " + cards3.amountOfNumerical(Ten))

        println("====================================")

        println("====== amountWithNumerical testing =======")

        println("Deck: " + cards3)
        println("Amount of numerical cards:  " + cards3.amountWithNumerical)

        println("====================================")

        println("====== amountOfFace testing =======")

        println("Deck: " + cards3)
        println("Face: " + Queen)
        println("Amount of cards:  " + cards3.amountOfFace(Queen))
        println("Face: " + King)
        println("Amount of cards:  " + cards3.amountOfFace(King))

        println("====================================")

        println("====== amountWithFace testing =======")

        println("Deck: " + cards3)
        println("Amount of face cards:  " + cards3.amountWithFace)

        println("====================================")

        println("====== Deck apply testing =======")

        val d1 = Deck.apply()
        println(d1)
        println("standard?: " + d1.isStandard)
        val faceA1 = d1.amountWithFace
        val numericalA1 = d1.amountWithNumerical
        println("face cards amount: " + faceA1)
        println("numerical cards amount: " + numericalA1)
        println("total cards amount: " + (faceA1 + numericalA1))

        val d2 = Deck.apply()
        println(d2)
        println("standard?: " + d2.isStandard)
        val faceA2 = d2.amountWithFace
        val numericalA2 = d2.amountWithNumerical
        println("face cards amount: " + faceA2)
        println("numerical cards amount: " + numericalA2)
        println("total cards amount: " + (faceA2 + numericalA2))

        println("====================================")

        println("====== Blackjack play testing =======")

        val blackjack = new Blackjack(d1)
        blackjack.play(3)

        println("====================================")


        println("====== Blackjack apply testing =======")

        val blackjack2 = Blackjack.apply(2)
        blackjack2.play(60) 

        println("====================================")

        println("====== Blackjack all21 testing =======")

        val blackjack3 = new Blackjack(d1)
        //println(blackjack3.all21)
        //blackjack3.first21()

        println("====================================")
    }
}