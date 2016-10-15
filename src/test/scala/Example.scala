import me.yuhuan.pacaya.playground._

/**
  * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 10/12/2016.
  */
object Example extends App {

  implicit val R = RealAlgebra
  implicit val BPStrategy = new BeliefPropagationStrategy {
    def schedule = BeliefPropagationSchedule.Treelike
    def updatingOrder = BeliefPropagationUpdatingOrder.Sequential
    def maxNumIteration = 100
    def shouldNormalizeMessages = true
    def convergenceThreshold = 1e-5
    def algebra = R
  }

  val x = BooleanVar("x")
  val x1 = BooleanVar("x1")
  val x2 = BooleanVar("x2")
  val y = BooleanVar("y")
  val y1 = BooleanVar("y1")
  val y2 = BooleanVar("y2")


  val fg = FactorGraph(
    Factor(x1)(R.one, R.fromReal(1)),
    Factor(x2)(R.one, R.fromReal(1)),
    IsAtMostOne2(x, x1, x2),
    Factor(y1)(R.one, R.fromReal(0.5)),
    Factor(y2)(R.one, R.one),
    IsAtMostOne2(y, y1, y2),
    Equal(y, x)
  )

  val lbp = BeliefPropagation(fg clampedOn VarConfig(x1 -> 1, y2 -> 0))
  lbp.run()

  val bS = lbp.marginalsOf(x)
  val bPos = lbp.marginalsOf(x1)
  val bNeg = lbp.marginalsOf(x2)
  val bA = lbp.marginalsOf(y)
  val bAgt = lbp.marginalsOf(y1)
  val bThm = lbp.marginalsOf(y2)

  val bp = 0
}
