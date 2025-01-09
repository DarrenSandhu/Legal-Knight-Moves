// Part 4 about finding a single tour on "mutilated" chessboards
//==============================================================

object M4d { 

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala or knight3.scala                  !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.

type Pos = (Int, Int)
type Path = List[Pos]

def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((i, j))}%4.0f ")
    }
    println()
  } 
}

// ADD YOUR CODE BELOW
//======================

def is_legal(dim: Int, path: Path, x: Pos, pred: Pos => Boolean) : Boolean = {
  (dim > x._1 && dim > x._2) && (x._1 >= 0 && x._2 >= 0) && (!path.contains(x)) && (pred(x))
}


//(2) 
def legal_moves(dim: Int, path: Path, x: Pos, pred: Pos => Boolean) : List[Pos] = {
  val moves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))
  val legal_moves = moves.filter(position => is_legal(dim, path, position, pred))
  legal_moves
}


//(6) 
def ordered_moves(dim: Int, path: Path, x: Pos, pred: Pos => Boolean) : List[Pos] = {
    val legalMoves = legal_moves(dim, path, x, pred)
    val eachLegalMoves = legalMoves.map(pos => (pos, legal_moves(dim, path :+ x, pos, pred)))
    val sizes = eachLegalMoves.map(moves => (moves._1, moves._2.size)).sortBy(_._2)
    val orderedPositions : List[Pos] = for (size <- sizes) yield {
        size._1
    }
    orderedPositions
}


def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = (xs, f) match {
  case (Nil, f) => None
  case (x :: xs, f) => 
    val result = f(x)
    if (result != None) result
    else first(xs, f)
}

// (10)
def one_tour_pred(dim: Int, path: Path, n: Int, pred: Pos => Boolean): Option[Path] = {
  if (path.length == n) Some(path)
  else {
    val ordered_moves = legal_moves(dim, path, path.head, pred)
    first(ordered_moves, pos => one_tour_pred(dim, path.::(pos), n, pred))
  }
}


}




// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
