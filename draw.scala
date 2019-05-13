case class RarityWeight(award: String, weight: Int)
val rarities = Seq(RarityWeight("GOLD", 10), RarityWeight("SILVER", 60), RarityWeight("BRONZE", 100))

def draw(rarities: Seq[RarityWeight]): String = {
  val distribution = rarities.foldLeft((0, Seq.empty[(RarityWeight,Int)])) { (res,item) =>
    val sum = res._1 + item.weight
    (sum, res._2 :+ (item, sum))
  }

  val s = distribution._2.find(_._2 > {
    val r = math.random
    (r * distribution._1).toInt
  }).get

  s._1.award
}


// draw!!
def test = {
  var i = 0
  do {
    println(draw(rarities))
    i = i + 1
  } while (i < 1000)
}
