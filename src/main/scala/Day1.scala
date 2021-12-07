
case class Input(file:String){
  val data = {
    val source = scala.io.Source.fromFile(file)
    try source.mkString.split("\n").toList finally source.close()
  }
}


object Day1 extends App{

  val data = Input("day1.txt").data
  def version1FirstPuzzle = {
    data.sliding(2).count{
      case List(a, b) => a.toInt < b.toInt
      case _ => false
    }
  }


  def version2FirstPuzzle = {

    val lines = data
    val zipped = for {
      ind <- lines.indices if ind != 0
    } yield lines(ind).toInt > lines(ind-1).toInt
    zipped.foldLeft(0)((sum,isBigger) => if (isBigger) sum + 1 else sum)
  }


  def v1SecondPuzzle = {
    data.sliding(4).count {
      case List(a,_,_,d) => a.toInt < d.toInt
      case _ => false
    }
  }




}
