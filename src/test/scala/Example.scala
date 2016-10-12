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


  val vS = BooleanVar("vS")
  val vPos = BooleanVar("vPos")
  val vNeg = BooleanVar("vNeg")
  val vA = BooleanVar("vA")
  val vAgt = BooleanVar("vAgt")
  val vThm = BooleanVar("vThm")


  val fg = FactorGraph(
    Factor(vPos)(R.one, R.fromReal(2)),
    Factor(vNeg)(R.one, R.fromReal(0.5)),
    TwoChooseOne(vS, vPos, vNeg),
    Factor(vAgt)(R.one, R.fromReal(0.5)),
    Factor(vThm)(R.one, R.fromReal(2.0)),
    TwoChooseOne(vA, vAgt, vThm),
    Equal(vA, vS)
  )

  val lbp = BeliefPropagation(fg)
  lbp.run()

  val bS = lbp.marginalsOf(vS)
  val bPos = lbp.marginalsOf(vPos)
  val bNeg = lbp.marginalsOf(vNeg)
  val bA = lbp.marginalsOf(vA)
  val bAgt = lbp.marginalsOf(vAgt)
  val bThm = lbp.marginalsOf(vThm)

  val bp = 0
}
