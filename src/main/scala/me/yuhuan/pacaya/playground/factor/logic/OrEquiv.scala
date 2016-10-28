package me.yuhuan.pacaya.playground.factor.logic

import me.yuhuan.pacaya.playground._

/**
  * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 10/28/2016.
  */
object OrEquiv {
  def apply(h: Var, b1: Var, b2: Var) = ExplicitFactor(h, b1, b2)(0,1,0,1,0,1,1,0)
}
