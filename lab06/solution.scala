// Marcin Bieganek

package pizzeria {
    package pizzaTypes {
        abstract class PizzaType {
            val price: Double
            override def toString() = this.getClass.getSimpleName
        }

        class Margarita extends PizzaType {
            val price = 5.00 // dollars
        }

        class Pepperoni extends PizzaType {
            val price = 6.50 // dollars
        }

        class Funghi extends PizzaType {
            val price = 7.00 // dollars
        }
    }

    package pizzaSizes {
        object PizzaSizes extends Enumeration {
            type PizzaSize = Value

            val Small, Regular, Large = Value
        }
    }

    package crustTypes {
        object CrustTypes extends Enumeration {
            type CrustType = Value

            val Thin, Thick = Value
        }
    }

    package pizzaToppings {
        abstract class PizzaTopping {
            val price: Double
            override def toString() = this.getClass.getSimpleName
        }

        class Ketchup extends PizzaTopping {
            val price = 0.50 // dollars
        }

        class Garlic extends PizzaTopping {
            val price = 0.50 // dollars
        }
    }

    package pizzaMeats {
        abstract class PizzaMeat {
            val price: Double
            override def toString() = this.getClass.getSimpleName
        }

        class Salami extends PizzaMeat {
            val price = 1.00 // dollars
        }
    }

    package drinks {
        abstract class Drink {
            val price: Double
            override def toString() = this.getClass.getSimpleName
        }

        class Lemonade extends Drink {
            val price = 2.00 // dollars
        }
    }
    
    package discounts {
        object Discounts extends Enumeration {
            type Discount = Value

            val Student, Senior = Value
        }
    }
    
    case class Pizza(
        ptype: pizzaTypes.PizzaType,
        size: pizzaSizes.PizzaSizes.PizzaSize,
        crust: crustTypes.CrustTypes.CrustType,
        extraMeat: Option[pizzaMeats.PizzaMeat],
        extraTopping: Option[pizzaToppings.PizzaTopping]
    ) {
        override def toString() = s"$size $ptype on $crust crust${extraMeat match { 
                                                                    case None => "" 
                                                                    case Some(m) => " with " + m}}${
                                                                  extraTopping match { 
                                                                    case None => "" 
                                                                    case Some(t) => " with " + t}}"

        val price: Double = {
            (size match {
                case pizzaSizes.PizzaSizes.Small => 0.9
                case pizzaSizes.PizzaSizes.Regular => 1.0
                case pizzaSizes.PizzaSizes.Large => 1.5
            }) * (ptype.price + 
                 (extraMeat match { 
                     case None => 0 
                     case Some(m) => m.price}) + 
                 (extraTopping match { 
                     case None => 0 
                     case Some(t) => t.price}))
        }
    }
}

package orders {
    import pizzeria._
    import pizzaTypes._
    import pizzaSizes.PizzaSizes._
    import crustTypes.CrustTypes._
    import pizzaMeats._
    import pizzaToppings._
    import drinks._
    import discounts.Discounts._

    class Order(
        name: String,
        address: String,
        phone: String, //mandatory validated phone-number (hint: regex)
        pizzas: List[Pizza],
        drinks: List[Drink],
        discount: Option[Discount], //optional value
        specialInfo: Option[String] //optional text, like: “Ring doesnt work, please knock”
    ) {
        private val phoneRegex = """^(\+48)?\d{9}$""".r
        require(!phoneRegex.findFirstIn(phone).isEmpty)

        override def toString() = s"Name: $name\n" +
                                  s"Address: $address\n" +
                                  s"Phone: $phone\n" +
                                  s"Pizzas: ${pizzas.mkString(", ")}\n" +
                                  s"Drinks: ${drinks.mkString(", ")}\n" + 
                                  s"discount: ${discount match {
                                                case None => "None"
                                                case Some(d) => d }}\n" + 
                                  s"specialInfo: ${specialInfo match {
                                                    case None => "None"
                                                    case Some(sI) => sI}}"
        
        private def priceOption(price: Double) = {
            price match {
                case 0.0 => None
                case x => Some(x)
            }
        }

        def extraMeatPrice: Option[Double] = priceOption(
            pizzas.map(p => 
                        (p.extraMeat match {
                            case None => 0.0
                            case Some(m) => m.price})).sum)

        def pizzasPrice: Option[Double] = priceOption(pizzas.map(p => p.price).sum)

        def drinksPrice: Option[Double] = priceOption(drinks.map(d => d.price).sum)

        def priceByType(ptype: PizzaType): Option[Double] = priceOption(
            pizzas.filter(p => p.ptype.getClass == ptype.getClass).map(p => p.price).sum
        )

        val price: Double = {
            discount match {
                case Some(Student) => (0.95 * pizzasPrice.getOrElse(0.0)) + drinksPrice.getOrElse(0.0)
                case Some(Senior) => 0.93 * (pizzasPrice.getOrElse(0.0) + drinksPrice.getOrElse(0.0))
                case _ => pizzasPrice.getOrElse(0.0) + drinksPrice.getOrElse(0.0)
            }
        }
    }
}

object MyApplication {
    def main(args: Array[String]) {
        import pizzeria._
        import pizzaTypes._
        import pizzaSizes.PizzaSizes._
        import crustTypes.CrustTypes._
        import pizzaMeats._
        import pizzaToppings._
        import drinks._
        import discounts.Discounts._
        import orders._
        
        println("========== pizza test 1 ==========")

        val p1 = new Pizza(
            new Margarita(),
            Small,
            Thin,
            None,
            None
        )

        println("pizza: " + p1)
        println("price: " + p1.price)
        
        println("==========================================")

        println("========== pizza test 2 ==========")

        val p2 = new Pizza(
            new Pepperoni(),
            Regular,
            Thin,
            Some(new Salami()),
            None
        )

        println("pizza: " + p2)
        println("price: " + p2.price)
        
        println("===========================================")

        println("========== pizza test 3 ==========")

        val p3 = new Pizza(
            new Funghi(),
            Large,
            Thick,
            Some(new Salami()),
            Some(new Ketchup())
        )

        println("pizza: " + p3)
        println("price: " + p3.price)
        
        println("============================================")

        def testOrder(o: Order) {
            println("order: ")
            println(o)

            println("\nextra meat price: " + o.extraMeatPrice)
            println("pizzas price: " + o.pizzasPrice)
            println("drinks price: " + o.drinksPrice)
            println("margarita price: " + o.priceByType(new Margarita()))
            println("pepperoni price: " + o.priceByType(new Pepperoni()))
            println("funghi price: " + o.priceByType(new Funghi()))
            println("order price: " + o.price)
        }

        println("========== order test 1 ==========")

        val o1 = new Order(
            "Bob",
            "ul. Kowalska 10, 50-500 Wroclaw",
            "123321121",
            List(p1, p2),
            List(),
            None,
            Some("Ring doesnt work, please knock")
        )
        testOrder(o1)

        println("===================================")

        println("========== order test 2 ==========")

        val o2 = new Order(
            "Sam",
            "ul. Kowalska 10, 50-500 Wroclaw",
            "123321121",
            List(),
            List(new Lemonade()),
            Some(Student),
            None
        )
        testOrder(o2)

        println("===================================")

        println("========== order test 3 ==========")

        val o3 = new Order(
            "Sam",
            "ul. Kowalska 10, 50-500 Wroclaw",
            "123321121",
            List(p3),
            List(new Lemonade()),
            Some(Student),
            None
        )
        testOrder(o3)

        println("===================================")

        println("========== order test 4 ==========")

        val o4 = new Order(
            "Sam",
            "ul. Kowalska 10, 50-500 Wroclaw",
            "123321121",
            List(p1, p2, p3),
            List(new Lemonade(), new Lemonade(), new Lemonade()),
            Some(Senior),
            None
        )
        testOrder(o4)

        println("===================================")

        println("========== order test 5 ==========")

        // Test with bad phone number
        // uncomment to test
        /*
        val o5 = new Order(
            "Sam",
            "ul. Kowalska 10, 50-500 Wroclaw",
            "abcd",
            List(p1, p2, p3),
            List(new Lemonade(), new Lemonade(), new Lemonade()),
            Some(Senior),
            None
        )
        testOrder(o5)
        */

        println("===================================")

    }
}