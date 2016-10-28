package me.yuhuan.pacaya.playground.factor.logic

import me.yuhuan.pacaya.playground.{ExplicitFactor, Var, VarConfig}
import edu.jhu.pacaya.{util => u}

/**
  * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 10/28/2016.
  */
object Xor {
  def apply(x: Var, y: Var)(implicit R: u.semiring.Algebra) = {
    val f = ExplicitFactor.ones(x, y)
    f(VarConfig(x->1, y->1)) = R.zero
    f(VarConfig(x->1, y->0)) = R.one
    f(VarConfig(x->0, y->1)) = R.one
    f(VarConfig(x->0, y->0)) = R.zero
    f
  }
}
