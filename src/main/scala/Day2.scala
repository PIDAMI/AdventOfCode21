object Day2 extends App{

  val data = Input("resources/day2.txt").data

  def v1 = {
    data.sliding(4).count {
        case List(a,_,_,d) => a < d
        case _ => false
    }
  }
  print(v1)

}
