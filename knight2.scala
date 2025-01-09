// Core Part about finding a single tour for a board using the
// Warnsdorf Rule
//==============================================================

object M4b {

// !!! Copy any function you need from file knight1.scala !!!
//
// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below.

type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

// ADD YOUR CODE BELOW
//======================

//(1) 
def is_legal(dim: Int, path: Path, x: Pos) : Boolean = {
  (dim > x._1 && dim > x._2) && (x._1 >= 0 && x._2 >= 0) && (!path.contains(x))
}


//(2) 
def legal_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
  val moves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))
  val legal_moves = moves.filter(position => is_legal(dim, path, position))
  legal_moves
}


//(6) 
def ordered_moves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val legalMoves = legal_moves(dim, path, x)
    val eachLegalMoves = legalMoves.map(pos => (pos, legal_moves(dim, path :+ x, pos)))
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

def checkAllMoves(dim: Int, path: Path, x: Pos) : List[Pos] = {
    val moves = List((x._1 + 1, x._2 + 2), (x._1 + 2, x._2 + 1), (x._1 + 2, x._2 - 1), (x._1 + 1, x._2 - 2), (x._1 - 1, x._2 - 2), (x._1 - 2, x._2 - 1), (x._1 - 2, x._2 + 1), (x._1 - 1, x._2 + 2))
    moves
}

//(7) 
def first_closed_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if ((path.size == dim * dim) && (checkAllMoves(dim, path, path.head).contains(path.last))) Some(path)
    else {
        if (path.size == dim * dim) None
        else {
            val orderedMoves = ordered_moves(dim, path, path.head)
            first(orderedMoves, pos => first_closed_tour_heuristics(dim, path.::(pos)))
        }
    }
}


//(8) Same as (7) but searches for *non-closed* tours. This 
//    version of the function will be called with dimensions of 
//    up to 30 * 30.

def first_tour_heuristics(dim: Int, path: Path) : Option[Path] = {
    if (path.size == dim * dim) Some(path)
    else {
        val orderedMoves = ordered_moves(dim, path, path.head)
        first(orderedMoves, pos => first_tour_heuristics(dim, path.::(pos)))
    }
}



}

// Test First Closed Tour Heuristics
//==================================
//
// You can test your code using the main function below.
// The main function will be used to test first_closed_tour_heuristics
// function. You can test the function by running the following command:
// scala M4b
// object Main {
//   def main(args: Array[String]) : Unit = {
//     val path = List((3, 3))
//     val dim = 6
//     val result = M4b.first_closed_tour_heuristics(dim, path)
//     println(result)
//   }
// }



// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
