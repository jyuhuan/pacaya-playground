package me.yuhuan.pacaya.playground.factor.logic

import me.yuhuan.pacaya.playground._
import edu.jhu.pacaya.{util => u}

/**
  * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 10/28/2016.
  */
object OrEquiv {
  def apply(hv: Var, bv1: Var, bv2: Var)(implicit R: u.semiring.Algebra) = {
    val f = ExplicitFactor.ones(hv, bv1, bv2)
    f(VarConfig(bv1->1, bv2->1, hv->1)) = R.one
    f(VarConfig(bv1->1, bv2->1, hv->0)) = R.zero
    f(VarConfig(bv1->1, bv2->0, hv->1)) = R.one
    f(VarConfig(bv1->1, bv2->0, hv->0)) = R.zero
    f(VarConfig(bv1->0, bv2->1, hv->1)) = R.one
    f(VarConfig(bv1->0, bv2->1, hv->0)) = R.zero
    f(VarConfig(bv1->0, bv2->0, hv->1)) = R.zero
    f(VarConfig(bv1->0, bv2->0, hv->0)) = R.one
    f
  }

}

object SlowOrEquivN {
  def apply(hv: Var)(bvs: Var*)(implicit R: u.semiring.Algebra) = ExplicitFactor.withConfigs(hv +: bvs: _*) { config =>
    val h = config(hv)
    val bs = bvs map config.stateIdOf

    var didViolate = false

    // L --> R, b1 b2 --> h
    val leftTrue = bs.sum >= 1
    val rightTrue = h == 1

    (leftTrue, rightTrue) match {
      case (true, true) => R.one
      case (false, false) => R.one
      case _ => R.zero
    }

  }
}

object OrEquivTest extends App {

  implicit val R = RealAlgebra

  val orN = SlowOrEquivN(BooleanVar("y"))(BooleanVar("x1"), BooleanVar("x2"))

  val or = OrEquiv(
    BooleanVar("y"),
    BooleanVar("x1"),
    BooleanVar("x2")
  )

  assert(orN.f.getValues sameElements or.f.getValues)

  val bp = 0

}