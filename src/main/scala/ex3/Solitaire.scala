package ex3

object Solitaire extends App:
  type Pos = (Int, Int)
  type Solution = Seq[Pos]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = LazyList(_)
  
  
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(w: Int, h: Int): Iterable[Solution] =
    
    def inGrid(p: Pos): Boolean = p._1 >= 0 && p._2 >= 0 && p._1 < w && p._2 < h

    def possibleNext(x: Int, y: Int): Seq[Pos] =
      val sDiagonal = Seq(0, -3, 3)
      val sPerpendicular = Seq(-2, 2)
      ( for
          dx <- sDiagonal
          dy <- sDiagonal
          if Math.abs(dx + dy) == 3
        yield (x + dx, y + dy)).concat(
        for
          dx <- sPerpendicular
          dy <- sPerpendicular
        yield (x + dx, y + dy)
      ).filter(inGrid)


    for
      p <- possibleNext(3, 2)
      n <- 1 until 35
    yield p

  val width = 7
  val height = 5
  placeMarks(width, height).foreach(s => println(render(s), width, height))
  
