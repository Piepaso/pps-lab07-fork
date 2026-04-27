package ex3

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(w: Int, h: Int): Seq[(Int, Int)] = {
    def inGrid(p: (Int, Int)): Boolean = p._1 >= 0 && p._2 >= 0 && p._1 < w && p._2 < h

    def possibleNext(p: (Int, Int)): Seq[(Int, Int)] =
      val sDiagonal = Seq(0, -3, 3)
      val sPerpendicular = Seq(-2, 2)
      ( for
          x <- sDiagonal
          y <- sDiagonal
          if Math.abs(x + y) == 3
        yield (p._1 + x, p._2 + y)).concat(
        for
          x <- sPerpendicular
          y <- sPerpendicular
        yield (p._1 + x, p._2 + y)
      )

    possibleNext(3, 2).filter(inGrid)
  }

  val width = 7
  val height = 5
  println(render(solution = placeMarks(width, height), width, height))
