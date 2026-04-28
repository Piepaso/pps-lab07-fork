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

  def findSolutions(width: Int, height: Int, startPos: Pos): Iterable[Solution] =

    def placeMarks(n: Int)(using factory: IterableFactory): Iterable[Solution] = n match
      case 1 => factory(Seq(startPos))
      case _ =>
        for
          solution <- placeMarks(n - 1)
          p <- possibleNext(solution.head)
          if !solution.contains(p)
        yield p +: solution

    def inGrid(p: Pos): Boolean = p._1 >= 0 && p._2 >= 0 && p._1 < width && p._2 < height

    def possibleNext(p: Pos): Seq[Pos] =
      val sPerpendicular = Seq(-3, 0, 3)
      val sDiagonal = Seq(-2, 2)
      def sumAll(l: Seq[Int], biPredicate: (Int, Int) => Boolean) = for
        dx <- l; dy <- l
        if biPredicate(dx, dy)
      yield (p._1 + dx, p._2 + dy)
      sumAll(sPerpendicular, (dx, dy) => Math.abs(dx + dy) == 3) ++ sumAll(sDiagonal, (_, _) => true) filter inGrid

    placeMarks(width * height)

  val width = 7
  val height = 5
  val startPos = (3, 2)
  findSolutions(width, height, startPos).take(5).foreach(sol => println("\n" + render(sol, width, height)))