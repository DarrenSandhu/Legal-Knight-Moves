// Main Part 4 about finding Knight's tours
//==========================================


object M4a {

// If you need any auxiliary functions, feel free to 
// implement them, but do not make any changes to the
// templates below. Also have a look whether the functions
// at the end of the file are of any help.

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
  val legalMoves = moves.filter(position => is_legal(dim, path, position))
  legalMoves
}

//some testcases

// assert(legal_moves(8, Nil, (2,2)) == 
//  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
// assert(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
// assert(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
//  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
// assert(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))


// (3) 
def count_tours(dim: Int, path: Path): Int = {
  val legalMovesCurrent = legal_moves(dim, path, path.head)
  legalMovesCurrent match {
      case Nil => if (path.length == dim * dim) 1 else 0
      case x :: xs => legalMovesCurrent.map(x => count_tours(dim, path.::(x))).sum
  }
}


def enum_tours(dim: Int, path: Path): List[Path] = {
    val legalMovesCurrent = legal_moves(dim, path, path.head)
    legalMovesCurrent match {
        case Nil => if (path.length == dim * dim) List(path) else List()
        case x :: xs => legalMovesCurrent.flatMap(x => enum_tours(dim, path.::(x)))
    }
}



// (4) 
def first(xs: List[Pos], f: Pos => Option[Path]) : Option[Path] = (xs, f) match {
  case (Nil, f) => None
  case (x :: xs, f) => 
    val result = f(x)
    if (result != None) result
    else first(xs, f)
}


// testcases
//
//def foo(x: (Int, Int)) = if (x._1 > 3) Some(List(x)) else None
//
//first(List((1, 0),(2, 0),(3, 0),(4, 0)), foo)   // Some(List((4,0)))
//first(List((1, 0),(2, 0),(3, 0)), foo)          // None


//(5) 
def first_tour(dim: Int, path: Path) : Option[Path] = {
  if (path.size == dim * dim) Some(path)
  else {
    val legalMoves = legal_moves(dim, path, path.head)
    first(legalMoves, pos => first_tour(dim, path.::(pos)))
  }
}

 


/* Helper functions


// for measuring time
def time_needed[T](code: => T) : T = {
  val start = System.nanoTime()
  val result = code
  val end = System.nanoTime()
  println(f"Time needed: ${(end - start) / 1.0e9}%3.3f secs.")
  result
}

// can be called for example with
//
//     time_needed(count_tours(dim, List((0, 0))))
//
// in order to print out the time that is needed for 
// running count_tours


// for printing a board
def print_board(dim: Int, path: Path): Unit = {
  println()
  for (i <- 0 until dim) {
    for (j <- 0 until dim) {
      print(f"${path.reverse.indexOf((j, dim - i - 1))}%3.0f ")
    }
    println()
  } 
}


*/

// Main method for testing
def main(args: Array[String]) : Unit = {
  println(legal_moves(8, Nil, (2,2)) == 
  List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
  println(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
  println(legal_moves(8, List((4,1), (1,0)), (2,2)) == 
  List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
  println(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))

  println()
  println(is_legal(8, Nil, (3, 4)) == true)
  println(is_legal(8, List((4, 1), (1, 0)), (4, 1)) == false)
  println(is_legal(2, Nil, (0, 0)) == true)

  println()
  println(legal_moves(8, Nil, (2,2)) == List((3,4), (4,3), (4,1), (3,0), (1,0), (0,1), (0,3), (1,4)))
  println(legal_moves(8, Nil, (7,7)) == List((6,5), (5,6)))
  println(legal_moves(8, List((4,1), (1,0)), (2,2)) == List((3,4), (4,3), (3,0), (0,1), (0,3), (1,4)))
  println(legal_moves(8, List((6,6)), (7,7)) == List((6,5), (5,6)))
  println(legal_moves(1, Nil, (0,0)) == Nil)
  println(legal_moves(2, Nil, (0,0)) == Nil)
  println(legal_moves(3, Nil, (0,0)) == List((1,2), (2,1)))

  println()
  println(enum_tours(5, List((0, 0))).length == 304)
  println(enum_tours(5, List((0,1)) ).length == 0)
  println(enum_tours(5, List((0,2)) ).length == 56)

}

}





// This template code is subject to copyright 
// by King's College London, 2022. Do not 
// make the template code public in any shape 
// or form, and do not exchange it with other 
// students under any circumstance.
