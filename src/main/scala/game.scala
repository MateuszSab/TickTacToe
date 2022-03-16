import scala.io.StdIn.readLine
import scala.sys.exit

class game {

  var board: Array[Array[Any]] = Array(Array(1, 2, 3), Array(4, 5, 6), Array(7, 8, 9))
  var lst_of_moves = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

  def isNumRight(n: String): Boolean = {
    n.toIntOption.isDefined && lst_of_moves.contains(n.toInt)
  }

  def getCol(n: Int, a: Array[Array[Int]]): Array[Int] = a.map {
    _ (n - 1)
  }


  println("Initial board")
  println(board.map(_.mkString).mkString("\n"))

  class playerXmove {
    def placeOfx(n: Int): Any = {
      if (n == 1) board(0)(0) = 'x'
      else if (n == 2) board(0)(1) = 'x'
      else if (n == 3) board(0)(2) = 'x'
      else if (n == 4) board(1)(0) = 'x'
      else if (n == 5) board(1)(1) = 'x'
      else if (n == 6) board(1)(2) = 'x'
      else if (n == 7) board(2)(0) = 'x'
      else if (n == 8) board(2)(1) = 'x'
      else if (n == 9) board(2)(2) = 'x'
      else println("Wrong number. Try again")
    }

    println("Player X - Choose a number of a place you want to fill with symbol x.")

    var x: String = readLine("Gracz |X| -- Type a chosen number: ")
    while (!isNumRight(x)) {
      println("ERROR")
      x = readLine("Gracz |X| -- Type a proper number: ")
    }
    placeOfx(x.toInt)
    lst_of_moves = lst_of_moves diff List(x.toInt)
    println(board.map(_.mkString).mkString("\n"))

  }

  class playerOmove {
    def placeOfx(n: Int): Any = {
      if (n == 1) board(0)(0) = 'o'
      else if (n == 2) board(0)(1) = 'o'
      else if (n == 3) board(0)(2) = 'o'
      else if (n == 4) board(1)(0) = 'o'
      else if (n == 5) board(1)(1) = 'o'
      else if (n == 6) board(1)(2) = 'o'
      else if (n == 7) board(2)(0) = 'o'
      else if (n == 8) board(2)(1) = 'o'
      else if (n == 9) board(2)(2) = 'o'
      else println("Wrong number. Try again")
    }

    println("Player O - Choose a number of a place you want to fill with symbol o.")


    var o: String = readLine("Gracz |O| -- Type a chosen number: ")
    while (!isNumRight(o)) {
      println("ERROR")
      o = readLine("Gracz |X| -- Type a proper number: ")
    }
    placeOfx(o.toInt)
    lst_of_moves = lst_of_moves diff List(o.toInt)
    println(board.map(_.mkString).mkString("\n"))

  }

  class checkIfXWon {
    if (board(0) sameElements Array('x', 'x', 'x')) {
      println("Player X WON!")
      exit()
    }
    else if (board(1) sameElements Array('x', 'x', 'x')) {
      println("Player X WON!")
      exit()
    }
    else if (board(2) sameElements Array('x', 'x', 'x')) {
      println("Player X WON!")
      exit()
    }
    else if (Array(board(0)(0), board(1)(1), board(2)(2)) sameElements Array('x', 'x', 'x')) {
      println("Player X WON!")
      exit()
    }
    else if (Array(board(0)(2), board(1)(1), board(2)(0)) sameElements Array('x', 'x', 'x')) {
      println("Player X WON!")
      exit()
    }
    else if (Array(board(0)(0), board(1)(0), board(2)(0)) sameElements Array('x', 'x', 'x')) {
      println("Player X WON!")
      exit()
    }
    else if (Array(board(0)(1), board(1)(1), board(2)(1)) sameElements Array('x', 'x', 'x')) {
      println("Player X WON!")
      exit()
    }
    else if (Array(board(0)(2), board(1)(2), board(2)(2)) sameElements Array('x', 'x', 'x')) {
      println("Player X WON!")
      exit()
    }

  }

  class checkIfOWon {
    if (board(0) sameElements Array('o', 'o', 'o')) {
      println("Player O WON!")
    }
    else if (board(1) sameElements Array('o', 'o', 'o')) {
      println("Player O WON!")
      exit()
    }
    else if (board(2) sameElements Array('o', 'o', 'o')) {
      println("Player O WON!")
      exit()
    }
    else if (Array(board(0)(0), board(1)(1), board(2)(2)) sameElements Array('o', 'o', 'o')) {
      println("Player O WON!")
      exit()
    }
    else if (Array(board(0)(2), board(1)(1), board(2)(0)) sameElements Array('o', 'o', 'o')) {
      println("Player O WON!")
      exit()
    }
    else if (Array(board(0)(0), board(1)(0), board(2)(0)) sameElements Array('o', 'o', 'o')) {
      println("Player O WON!")
      exit()
    }
    else if (Array(board(0)(1), board(1)(1), board(2)(1)) sameElements Array('o', 'o', 'o')) {
      println("Player O WON!")
      exit()
    }
    else if (Array(board(0)(2), board(1)(2), board(2)(2)) sameElements Array('o', 'o', 'o')) {
      println("Player O WON!")
      exit()
    }
  }

  while (lst_of_moves.nonEmpty) {
    new playerXmove
    new checkIfXWon
    if (lst_of_moves.isEmpty) {
      println("End of moves. DRAW")
      exit()
    }
    new playerOmove
    new checkIfOWon
  }
}