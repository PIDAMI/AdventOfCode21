object Day2 extends App{


  case class Position(horz:Int,depth:Int){
    def apply(delta:Position) =
      Position(horz + delta.horz,depth + delta.depth)
    def toInt = horz * depth
  }

  def part1 = Input("resources/day2.txt").data.map{ x =>
    x.split(" ")(0) match {
      case "forward" => Position(x.split(" ")(1).toInt, 0)
      case "down" => Position(0, x.split(" ")(1).toInt)
      case "up" => Position(0, -x.split(" ")(1).toInt)
    }
  }.foldLeft(Position(0,0))(_.apply(_)).toInt



  def part2 = {
    var aim = 0
    var pos = 0
    var depth = 0
    Input("resources/day2.txt").data.foreach{ x =>
      x.split(" ")(0) match {
        case "forward" =>
          pos += x.split(" ")(1).toInt;
          depth += x.split(" ")(1).toInt * aim
        case "down" => aim += 1
        case "up" => aim -= 1
      }
    }
    depth * pos
  }

  print(part2)



}
