import math.pow
object Day3 extends App{

  val data = Input("resources/day3.txt").data
    .map(_.split("").map(_.toInt))
    .transpose
    .map(x => if (2 * x.sum > x.length ) 1 else 0)
    .reverse

  val gamma = {
    for {ind <- data.indices} yield pow(2, ind).toInt * data(ind)
  }.sum

  val eps = pow(2,data.length).toInt - gamma - 1

  println(f"result:${eps * gamma}")

}
