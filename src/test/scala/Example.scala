/**
  * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 10/12/2016.
  */
object Example extends App {

  import me.yuhuan.pacaya.playground._

  implicit val R = LogSignAlgebra
  implicit val BPStrategy = new BeliefPropagationStrategy {
    def schedule = BeliefPropagationSchedule.Treelike
    def updatingOrder = BeliefPropagationUpdatingOrder.Sequential
    def maxNumIteration = 100
    def shouldNormalizeMessages = true
    def convergenceThreshold = 1e-5
    def algebra = R
  }

  val vS = BooleanVar("s")
  val vP = BooleanVar("p")
  val vN = BooleanVar("n")

  val fP = Factor(vS)(R.one, R.fromReal(5.0))
  val fN = Factor(vP)(R.one, R.one)
  val fIsExactly1 = Factor.withConfigs(vS, vP, vN){ conf =>
    (conf(vS), conf(vP), conf(vN)) match {
      case (0, 0, 0) => R.one
      case (0, 0, 1) => R.zero
      case (0, 1, 0) => R.zero
      case (0, 1, 1) => R.one
      case (1, 0, 0) => R.zero
      case (1, 0, 1) => R.one
      case (1, 1, 0) => R.zero
      case (1, 1, 1) => R.zero
    }
  }

  val fg = FactorGraph(fP, fN, fIsExactly1)

  val id1 = vS.v.getId
  val id2 = vP.v.getId
  val id3 = vN.v.getId


  val lbp = BeliefPropagation(fg)
  lbp.run()

  val b1 = lbp.marginalsOf(vS)
  val b2 = lbp.marginalsOf(vP)
  val b3 = lbp.marginalsOf(vN)


  println(fg.fg)

  val bp = 0


}
