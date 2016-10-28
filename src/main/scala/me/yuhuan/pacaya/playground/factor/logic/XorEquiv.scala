package me.yuhuan.pacaya.playground.factor.logic

import me.yuhuan.pacaya.playground._
import edu.jhu.pacaya.{util => u}

/**
  * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 10/28/2016.
  */
object XorEquiv {
  def apply(hv: Var, bv1: Var, bv2: Var)(implicit R: u.semiring.Algebra) = {
    val f = ExplicitFactor.ones(hv, bv1, bv2)
    f(VarConfig(bv1->1, bv2->1, hv->1)) = R.zero
    f(VarConfig(bv1->1, bv2->1, hv->0)) = R.one
    f(VarConfig(bv1->1, bv2->0, hv->1)) = R.one
    f(VarConfig(bv1->1, bv2->0, hv->0)) = R.zero
    f(VarConfig(bv1->0, bv2->1, hv->1)) = R.one
    f(VarConfig(bv1->0, bv2->1, hv->0)) = R.zero
    f(VarConfig(bv1->0, bv2->0, hv->1)) = R.zero
    f(VarConfig(bv1->0, bv2->0, hv->0)) = R.one
    f
  }
}


