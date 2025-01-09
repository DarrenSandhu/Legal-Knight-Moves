// Finding a single tour on a "mega" board
//=========================================

object M4c {

// !!! Copy any function you need from file knight1.scala !!!
// !!! or knight2.scala                                   !!! 
//
// If you need any auxiliary function, feel free to 
// implement it, but do not make any changes to the
// templates below.


type Pos = (Int, Int)    // a position on a chessboard 
type Path = List[Pos]    // a path...a list of positions

// ADD YOUR CODE BELOW
//======================

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

//(9) 

import scala.annotation.tailrec
      
def tour_on_mega_board(dim: Int, path: Path) : Option[Path] = {
    @tailrec
    def helper(path: Path, moves: List[Pos]): Option[Path] = moves match {
        case Nil => if (path.size == dim * dim) Some(path) else None
        case head :: tail => 
            val newPath = path.::(head)
            if (newPath.size == dim * dim) Some(newPath)
            else helper(newPath, ordered_moves(dim, newPath, newPath.head))
    }
    helper(path, ordered_moves(dim, path, path.head))
}

}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
