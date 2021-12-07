case class Input(file:String){
  val data = {
    val source = scala.io.Source.fromFile(file)
    try source.mkString.split("\n").toList.map(_.toInt) finally source.close()
  }
}


object Day1 extends App{

  val input = Input("day1.txt")
  def version1 = {
    input.data.sliding(2).count{
      case List(a, b) => a < b
      case _ => false
    }
  }


  def version2 = {

    val lines = input.data
    val zipped = for {
      ind <- lines.indices if ind != 0
    } yield lines(ind) > lines(ind-1)
    zipped.foldLeft(0)((sum,isBigger) => if (isBigger) sum + 1 else sum)
  }

  println(version1)
  println(version2)

}
