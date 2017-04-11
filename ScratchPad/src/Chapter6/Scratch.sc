val data = 1 to 2

for(x <- data; y <- data ) yield List(x,y)



object Distribution {
  def uniform[A](atoms: List[A]): Distribution[A] = {
    val p = 1.0 / atoms.length
    Distribution(atoms.map(a => a -> p))
  }

  def discrete[A](events: List[(A,Double)]): Distribution[A] ={
    Distribution(events).compact.normalize}
}


final case class Distribution[A](events: List[(A, Double)]) {
  def map[B](f: A => B): Distribution[B] =
    Distribution(events map { case (a, p) => f(a) -> p })

  def flatMap[B](f: A => Distribution[B]): Distribution[B] =
    Distribution(events flatMap { case (a, p1) =>
      f(a).events map { case (b, p2) => b -> (p1 * p2) }
    }).compact.normalize

  def normalize: Distribution[A] = {
    val totalWeight = (events map { case (a, p) => p }).sum
    Distribution(events map { case (a,p) => a -> (p / totalWeight) })
  }

  def compact: Distribution[A] = {
    val distinct = (events map { case (a, p) => a }).distinct
    def prob(a: A): Double =
      (events filter { case (x, p) => x == a } map { case (a, p) => p }).sum

    Distribution(distinct map { a => a -> prob(a) })
  }


}
sealed trait Coin
final case object Heads extends Coin
final case object Tails extends Coin

val fairCoin: Distribution[Coin] = Distribution.uniform(List(Heads, Tails))
val threeFlips =
  for {
    c1 <- fairCoin
    c2 <- fairCoin
    c3 <- fairCoin
  } yield (c1,c2,c3)

// threeFlips: Distribution[(Coin, Coin, Coin)] =
// Distribution(List(
//    ((Heads,Heads,Heads),0.125),
//    ((Heads,Heads,Tails),0.125),
//    ((Heads,Tails,Heads),0.125),
//    ((Heads,Tails,Tails),0.125),
//    ((Tails,Heads,Heads),0.125),
//    ((Tails,Heads,Tails),0.125),
//    ((Tails,Tails,Heads),0.125),
//    ((Tails,Tails,Tails),0.125)
// ))”
/*

sealed trait Food
final case object Raw extends Food
final case object Cooked extends Food

val food: Distribution[Food] =
  Distribution.discrete(List(Cooked -> 0.3, Raw -> 0.7))

sealed trait Cat
final case object Asleep extends Cat
final case object Harassing extends Cat

def cat(food: Food): Distribution[Cat] =
  food match {
    case Cooked => Distribution.discrete(List(Harassing -> 0.8, Asleep -> 0.2))
    case Raw => Distribution.discrete(List(Harassing -> 0.4, Asleep -> 0.6))
  }

val foodModel: Distribution[(Food, Cat)] =
  for {
    f <- food
    c <- cat(f)
  } yield (f, c)
*/