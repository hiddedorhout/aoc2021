import File.readFile
import cats.Foldable
import cats.implicits._

object AocPlaybook extends App {

  //day 1
  println("DAY 1")

  val depthReport = readFile[Int]("depthReport")(_.toInt)

  def countIncreases(records: List[Int], windowSize: Int = 1): Int = {
    records.sliding(windowSize).toList.headOption match {
      case Some(firstWindow) if records.length >= windowSize =>
        Foldable[List]
          .foldLeft(records.sliding(windowSize).toList, (0, firstWindow.combineAll)) {
            case ((count, previousWindow), currentWindow) =>
              if (previousWindow < currentWindow.combineAll) (count + 1, currentWindow.combineAll)
              else (count, currentWindow.combineAll)
          }
          ._1
      case None => 0
    }
  }

//  println(s"Number of increases in depth is ${countIncreases(depthReport)}")
//  println(s"Number of window sum increases in depth is ${countIncreases(depthReport, 3)}")

  // day2
  println("DAY 2")

  val horizontalStart = 0
  val depthStart      = 0

  case class Step(direction: Direction, value: Int)

  sealed trait Direction
  case object Up      extends Direction
  case object Down    extends Direction
  case object Forward extends Direction

  def decodeDepth(in: String): Step = {
    val p               = in.split(" ").toList
    val directionString = p.head
    val value           = p.tail.head.toInt
    directionString match {
      case "forward" => Step(Forward, value)
      case "up"      => Step(Up, value)
      case "down"    => Step(Down, value)
    }
  }

  val course = readFile("course")(decodeDepth)

  val result_part1 = course
    .foldLeft((horizontalStart, depthStart))((hd, step) => {
      val (horizontalLocation, depthLocation) = hd
      step.direction match {
        case Up      => (horizontalLocation, depthLocation - step.value)
        case Down    => (horizontalLocation, depthLocation + step.value)
        case Forward => (horizontalLocation + step.value, depthLocation)
      }
    })

//  println(s"The sum of the location is: ${result_part1._1 * result_part1._2}")

  val aimStart = 0

  val result_part2 = course
    .foldLeft((horizontalStart, depthStart, aimStart))((hda, step) => {
      val (horizontalLocation, depthLocation, aim) = hda
      step.direction match {
        case Up      => (horizontalLocation, depthLocation, aim - step.value)
        case Down    => (horizontalLocation, depthLocation, aim + step.value)
        case Forward => (horizontalLocation + step.value, depthLocation + (step.value * aim), aim)
      }
    })

//  println(s"The sum of the location based on aim is: ${result_part2._1 * result_part2._2}")

  // day3
  println("DAY 3")

  def diagnosticDecoder(in: String): List[String] = in.split("").toList

  val diagnosticReport = readFile("diagnosticReport")(diagnosticDecoder)

  lazy val powerConsumption = diagnosticReport.transpose
    .map(getMostAndLeastAvailable)
    .transpose
    .map(listOfBinaryValuesToInt)
    .product

  def getMostAndLeastAvailable(l: List[String]): List[String] = {
    if (l.count(_ == "0") > l.count(_ == "1")) List("0", "1") else List("1", "0")
  }

  def getMostAndLeastAvailable1(l: List[String]): List[String] = {
    if (l.count(_ == "1") > l.count(_ == "0")) List("0", "1") else List("1", "0")
  }

  def listOfBinaryValuesToInt(in: List[String]): Int = Integer.parseInt(in.mkString(""), 2)

//  println(s"The power consumption of the submarine is ${powerConsumption}")

  def toFilteredList(next: List[List[String]],
                     criteriaOps: List[List[String]] => List[String])(index: Int): List[List[String]] = {
    if (next.length != 1) {
      val filtered = next.filter(l => {
        val indexToCheck = l.length - index
        l(indexToCheck) == criteriaOps(next)(indexToCheck)
      })
      toFilteredList(filtered, criteriaOps)(index - 1)
    } else {
      next
    }
  }

  def oxygenCriteria(in: List[List[String]]): List[String] =
    in.transpose.map(l => {
      if (l.count(_ == "0") > l.count(_ == "1")) "0" else "1"
    })

  def co2Criteria(in: List[List[String]]): List[String] =
    in.transpose.map(l => {
      if (l.count(_ == "1") >= l.count(_ == "0")) "0" else "1"
    })

  lazy val oxygenRating = listOfBinaryValuesToInt(
    toFilteredList(diagnosticReport, oxygenCriteria)(diagnosticReport.head.length).head
  )
  lazy val co2Rating = listOfBinaryValuesToInt(
    toFilteredList(diagnosticReport, co2Criteria)(diagnosticReport.head.length).head
  )
//  println(s"The life support rating of the submarine is ${oxygenRating * co2Rating}")

  // DAY 4
  println("DAY 4")
  type Board = List[List[(Int, Boolean)]]

  val (draws, flatBoards) =
    readFile("bingo")(identity).foldLeft(List.empty[Int], List.empty[List[Int]])((drawsBoards, line) => {
      val (draws, boards) = drawsBoards
      if (draws.isEmpty) {
        (line.split(",").toList.map(_.toInt), boards)
      } else {
        if (line.isEmpty) {
          drawsBoards
        } else {
          val cleanLine = line.trim.split("\\s+").toList
          (draws, cleanLine.map(_.toInt) :: boards)
        }
      }
    })

  val boards = flatBoards.reverse.sliding(5, 5).toList

//  println(s"n boards: ${boards.length}")

  val statusBoard: List[Board] = boards.map(board => {
    board.map(_.map((_, false)))
  })

  def updateBoard(board: Board, draw: Int): Board = {
    board.map(_.map(n => {
      if (n._1 == draw) n.copy(_2 = true) else n
    }))
  }

  def boardScore(board: Board): Option[Int] = {
    val columnScore = board.transpose
      .map(column => {
        if (column.map(_._2).forall(_ == true)) {
          Some(calculateBoardSum(board))
        } else {
          None
        }
      })
      .combineAll
    val rowScore = board
      .map(row => {
        if (row.map(_._2).forall(_ == true)) {
          Some(calculateBoardSum(board))
        } else {
          None
        }
      })
      .combineAll
    List(columnScore, rowScore).combineAll
  }

  def calculateBoardSum(board: Board): Int = {
    board.map(row => row.filterNot(_._2)).flatMap(_.map(_._1)).sum
  }

  case class WinningBoard(id: Int, score: Int, board: Board)

  lazy val bingoScore = draws.foldLeft((statusBoard, List.empty[WinningBoard]))((boardsResult, draw) => {
    val (boards, winningBoards) = boardsResult
    val listOfBoardResults = boards.zipWithIndex.map(boardId => {
      val (board, id)  = boardId
      val updatedBoard = updateBoard(board, draw)
      boardScore(updatedBoard) match {
        case Some(score) => {
          if (!winningBoards.map(_.id).contains(id)) {
            val winner = WinningBoard(id, score * draw, updatedBoard)
            (updatedBoard, winner :: winningBoards)
          } else {
            (updatedBoard, winningBoards)
          }
        }
        case None => (updatedBoard, winningBoards)
      }
    })

    listOfBoardResults.foldLeft((listOfBoardResults.map(_._1), List.empty[WinningBoard]))(
      (boardsAndScores, boardResult) => {
        val winningBoards = boardResult._2
          .foldLeft(List.empty[WinningBoard])((acc, winner) => {
            if (!boardsAndScores._2.map(_.board).contains(winner.board)) {
              winner :: acc
            } else {
              acc
            }
          })
          .reverse
        val bb = winningBoards ::: boardsAndScores._2
        (boardsAndScores._1, bb)
      }
    )

  })

//  println(s"The first board to win has score ${bingoScore._2.reverse.head.score}")
//  println(s"The last board to win has score ${bingoScore._2.head.score}")

  // Day 5
  println("DAY 5")

  type Coordinate = (Int, Int)

  implicit def horizontalVerticalFiler(line: (Coordinate, Coordinate)): List[Coordinate] = {
    val x1 = line._1._1
    val y1 = line._1._2
    val x2 = line._2._1
    val y2 = line._2._2

    val isHorizontal = x1 == x2
    val isVertical   = y1 == y2

    (isHorizontal, isVertical) match {
      case (true, false) =>
        if (y1 > y2) {
          y2.to(y1).toList.map(y => (x1, y))
        } else {
          y1.to(y2).toList.map(y => (x1, y))
        }
      case (false, true) =>
        if (x1 > x2) {
          x2.to(x1).toList.map(x => (x, y1))
        } else {
          x1.to(x2).toList.map(x => (x, y1))
        }
      case _ => List.empty
    }
  }

  implicit def diagonalFiler(line: (Coordinate, Coordinate)): List[Coordinate] = {
    val x1 = line._1._1
    val y1 = line._1._2
    val x2 = line._2._1
    val y2 = line._2._2

    val isHorizontal = x1 == x2
    val isVertical   = y1 == y2
    val isDiagonal   = if (isHorizontal || isVertical) false else (y2 - y1).abs / (x2 - x1).abs == 1

    (isHorizontal, isVertical, isDiagonal) match {
      case (true, false, false) =>
        if (y1 > y2) {
          y2.to(y1).toList.map(y => (x1, y))
        } else {
          y1.to(y2).toList.map(y => (x1, y))
        }
      case (false, true, false) =>
        if (x1 > x2) {
          x2.to(x1).toList.map(x => (x, y1))
        } else {
          x1.to(x2).toList.map(x => (x, y1))
        }
      case (false, false, true) =>
        (x1 > x2, y1 > y2) match {
          case (true, true)   => x2.to(x1).toList.zip(y2.to(y1))
          case (false, false) => x1.to(x2).toList.zip(y1.to(y2))
          case (true, false)  => x2.to(x1).toList.reverse.zip(y1.to(y2))
          case (false, true)  => x1.to(x2).toList.zip(y2.to(y1).reverse)
        }
      case _ => List.empty
    }
  }

  def hvDecoder(line: String): List[Coordinate] = {
    val start = line.split(" -> ").head.split(",")
    val end   = line.split(" -> ").reverse.head.split(",")
    val x1    = start.head.toInt
    val y1    = start.reverse.head.toInt
    val x2    = end.head.toInt
    val y2    = end.reverse.head.toInt
    horizontalVerticalFiler((x1, y1), (x2, y2))
  }
  def hvvDecoder(line: String): List[Coordinate] = {
    val start = line.split(" -> ").head.split(",")
    val end   = line.split(" -> ").reverse.head.split(",")
    val x1    = start.head.toInt
    val y1    = start.reverse.head.toInt
    val x2    = end.head.toInt
    val y2    = end.reverse.head.toInt
    horizontalVerticalFiler((x1, y1), (x2, y2))
  }

  val horizontalVerticalVentLocations         = readFile("vents")(value => hvDecoder(value)).flatten
  val horizontalVerticalDiagonalVentLocations = readFile("vents")(value => hvvDecoder(value)).flatten

  def collisions(ventLocations: List[Coordinate]): Int =
    ventLocations
      .foldLeft((Set.empty[Coordinate], List.empty[Coordinate]))((acc, line) => {
        if (line.isEmpty) {
          acc
        } else {
          if (acc._2.contains(line)) {
            (acc._1 + line, line :: acc._2)
          } else {
            acc.copy(_2 = line :: acc._2)
          }
        }
      })
      ._1
      .toList
      .length

//  println(
//    s"The Horizontal and Vertical amount of points where at least two lines overlap: ${collisions(horizontalVerticalVentLocations)}"
//  )
//  println(
//    s"The Horizontal, Vertical and Diagonal amount of points where at least two lines overlap: ${collisions(horizontalVerticalDiagonalVentLocations)}"
//  )

//  day 6
  println("DAY 6")

  val initialFishCount = readFile("fish")(_.split(",").map(_.toInt)).flatten
  val days             = 80

  def evolveFish(daysUntilNewFish: List[Int], day: Int): List[Int] = {
    if (day != 0) {
      daysUntilNewFish.flatMap(a => {
        if (a == 0) {
          evolveFish(List(6, 8), day - 1)
        } else {
          evolveFish(List(a - 1), day - 1)
        }
      })
    } else {
      daysUntilNewFish
    }
  }

  println(s"There are ${evolveFish(initialFishCount, days).length} fishes after $days days")

}
