class Pouring(capacities: Vector[Int]) {
  // states
  type State = Vector[Int]
  val initialState = Vector.fill(capacities.length)(0)

  // moves are things you can do to glasses
  trait Move {
    def change(state: State): State
  }

  case class Empty(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, 0)
  }

  case class Fill(glass: Int) extends Move {
    override def change(state: State): State = state updated (glass, capacities(glass))
  }

  case class Pour(from: Int, to: Int) extends Move {
    override def change(state: State): State = {
      // we can pour either the whole contents of the "from" glass or
      // the remaining amount in the "to" glass, whichever is smaller
      val amountCanPour = state(from) min (capacities(to) - state(to))
      state updated (from, state(from) - amountCanPour) updated (to, state(to) + amountCanPour)
    }
  }

  val glasses = capacities.indices

  val moves = (for {g <- glasses} yield Empty(g)) ++
    (for {g <- glasses} yield Fill(g)) ++
    (for {from <- glasses
          to <- glasses if from != to} yield Pour(from, to))

  class Path(history: List[Move], val endState: State) {
    def extend(move: Move) = new Path(move :: history, move change endState)

    override def toString: String = (history.reverse mkString " ") + " --> " + endState
  }

  val initialPath = new Path(Nil, initialState)

  /**
    *
    * @param paths
    * @param explored
    * @return
    */
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] =
    if (paths.isEmpty) Stream.empty
    else {
      val more = for {
        path <- paths
        next <- moves map path.extend
        if !(explored contains next.endState)
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }

  val pathSets = from(Set(initialPath), Set(initialState))
  def solutions(target: Int): Stream[Path] =
    for {
      pathSet <- pathSets
      path <- pathSet
      if path.endState contains target
    } yield path
}


  val problem = new Pouring(Vector(4, 9, 5))
  problem.moves
problem.pathSets.take(3).toList

problem.solutions(6)
