package me.yuhuan.pacaya

import scala.math._
import scala.collection.JavaConversions._
import edu.jhu.{pacaya => p}
import edu.jhu.pacaya.gm.{model => m}
import edu.jhu.pacaya.gm.{data => d}
import edu.jhu.pacaya.{util => u}
import edu.jhu.pacaya.gm.{inf => i}

/**
  * Created by Yuhuan Jiang (jyuhuan@gmail.com) on 10/11/2016.
  */
package object playground {

  //region Common variable domains

  val BooleanDomain = Seq("F", "T")

  //endregion


  //region Common algebras for belief propagation

  val LogSignAlgebra = u.semiring.LogSignAlgebra.getInstance()
  val RealAlgebra = u.semiring.RealAlgebra.getInstance()
  val LogSemiring = u.semiring.LogSemiring.getInstance()

  //endregion


  //region Sweeteners for variables

  implicit class Var(val v: m.Var) extends AnyVal {
    def name: String = v.getName
    def states: Seq[String] = v.getStateNames
  }
  object Var {
    def apply(name: String)(states: String*): Var = PredictedVar(name)(states: _*)
  }
  object BooleanVar {
    def apply(name: String): Var = BooleanPredictedVar(name)
  }

  object PredictedVar {
    def apply(name: String)(states: String*): Var = new m.Var(m.Var.VarType.PREDICTED, states.length, name, states)
  }
  object BooleanPredictedVar {
    def apply(name: String): Var = PredictedVar(name)(BooleanDomain: _*)
  }

  object LatentVar {
    def apply(name: String)(states: String*): Var = new m.Var(m.Var.VarType.LATENT, states.length, name, states)
  }
  object BooleanLatentVar {
    def apply(name: String): Var = LatentVar(name)(BooleanDomain: _*)
  }

  implicit class VarSet(val vs: m.VarSet) extends AnyVal
  object VarSet {
    def apply(vs: Var*): VarSet = new m.VarSet(vs.map(_.v): _*)
  }

  implicit class VarConfig(val vc: m.VarConfig) extends AnyVal {
    def stateIdOf(v: Var) = vc.getState(v.v)
    def stateOf(v: Var) = vc.getStateName(v.v)
    def vars: VarSet = vc.getVars
    def apply(v: Var): Int = stateIdOf(v)
  }

  //endregion


  //region Sweeteners for factors

  implicit class VarTensor(val t: m.VarTensor) extends AnyVal {
    def vars: VarSet = t.getVars
    def argmaxConfigId: Int = t.getArgmaxConfigId
  }

  implicit class Factor(val f: m.Factor) extends AnyVal {
    def logScore(configIdx: Int): Double = f.getLogUnormalizedScore(configIdx)
    def score(configIdx: Int): Double = exp(f.getLogUnormalizedScore(configIdx))
  }

  object Factor {
    def apply(vs: Var*)(rs: Double*)(implicit R: u.semiring.Algebra) = ExplicitFactor(vs: _*)(rs: _*)(R)
    def tabulate(vs: Var*)(f: Int => Double)(implicit R: u.semiring.Algebra) = ExplicitFactor.tabulate(vs: _*)(f)(R)
    def withConfigs(vs: Var*)(f: VarConfig => Double)(implicit R: u.semiring.Algebra) = ExplicitFactor.withConfigs(vs: _*)(f)(R)
  }


  implicit class ExplicitFactor(val f: m.ExplicitFactor) extends AnyVal {
    def update(idx: Int, v: Double) = f.setValue(idx, v)
  }

  /**
    * Fixes an important issue in the original Pacaya library.
    * See <a href="https://github.com/mgormley/pacaya/issues/2#issuecomment-253317931">Issue#2</a>.
    */
  object ExplicitFactor {
    def apply(vs: Var*)(rs: Double*)(implicit R: u.semiring.Algebra): ExplicitFactor = {
      val result = new m.ExplicitFactor(VarSet(vs: _*).vs)
      var i = 0; while (i < result.size) {
        val logValue = R.toLogProb(rs(i))
        result.setValue(i, logValue)
        i += 1
      }
      result
    }

    def tabulate(vs: Var*)(f: Int => Double)(implicit R: u.semiring.Algebra): ExplicitFactor = {
      val result = new m.ExplicitFactor(VarSet(vs: _*).vs)
      var i = 0; while (i < result.size) {
        result.setValue(i, f(i))
        i += 1
      }
      result
    }

    def withConfigs(vs: Var*)(f: VarConfig => Double)(implicit R: u.semiring.Algebra): ExplicitFactor = {
      val result = new m.ExplicitFactor(VarSet(vs: _*).vs)
      val vars = result.getVars
      var configIdx = 0; while (configIdx < result.size) {
        val config = vars.getVarConfig(configIdx)
        result.setValue(configIdx, f(config))
        configIdx += 1
      }
      result
    }
  }
  implicit def ExplicitFactorIsFactor(ef: ExplicitFactor): Factor = new Factor(ef.f)

  //endregion


  //region Factor graphs

  implicit class FactorGraph(val fg: m.FactorGraph) extends AnyVal {

  }
  object FactorGraph {
    def apply(fs: Factor*): FactorGraph = {
      val fg = new m.FactorGraph
      fs.foreach(f => fg addFactor f.f)
      fg
    }
  }

  //endregion



  //region LBP

  implicit class BeliefPropagation(val bp: i.BeliefPropagation) extends AnyVal {
    def run(): Unit = bp.run()
    def marginalsOf(v: Var): VarTensor = bp.getMarginals(v.v)
    def marginalsOf(f: Factor): VarTensor = bp.getMarginals(f.f)
  }
  object BeliefPropagation {
    def apply(fg: FactorGraph)(implicit S: BeliefPropagationStrategy): BeliefPropagation = {
      val prm = new i.BeliefPropagation.BeliefPropagationPrm {
        schedule = S.schedule match {
          case BeliefPropagationSchedule.Random => i.BeliefPropagation.BpScheduleType.RANDOM
          case BeliefPropagationSchedule.Treelike => i.BeliefPropagation.BpScheduleType.TREE_LIKE
        }
        updateOrder = S.updatingOrder match {
          case BeliefPropagationUpdatingOrder.Parallel => i.BeliefPropagation.BpUpdateOrder.PARALLEL
          case BeliefPropagationUpdatingOrder.Sequential => i.BeliefPropagation.BpUpdateOrder.SEQUENTIAL
        }
        maxIterations = S.maxNumIteration
        normalizeMessages = S.shouldNormalizeMessages
        convergenceThreshold = S.convergenceThreshold
        s = S.algebra
      }
      val bp = new i.BeliefPropagation(fg.fg, prm)
      bp
    }
  }

  trait BeliefPropagationStrategy {
    def schedule: BeliefPropagationSchedule
    def updatingOrder: BeliefPropagationUpdatingOrder
    def maxNumIteration: Int
    def shouldNormalizeMessages: Boolean
    def convergenceThreshold: Double
    def algebra: u.semiring.Algebra
  }

  //endregion


  //region Everything about training

  implicit class MemoryExampleStore(val es: d.FgExampleMemoryStore) extends AnyVal
  object MemoryExampleStore {
    def apply(es: LabeledExample*): MemoryExampleStore = {
      val result = new d.FgExampleMemoryStore
      es.foreach(e => result.add(e.e))
      result
    }
  }

  implicit class DiskExampleStore(val es: d.FgExampleDiskStore) extends AnyVal

  implicit class LabeledExample(val e: d.LabeledFgExample) extends AnyVal
  implicit class UnlabeledExample(val e: d.UnlabeledFgExample) extends AnyVal

  //endregion


  //region Convenient factors

  def Equal(x1: Var, x2: Var)(implicit R: u.semiring.Algebra) = ExplicitFactor.withConfigs(x1, x2){ conf =>
    (conf(x1), conf(x2)) match {
      case (0, 0) => R.one
      case (0, 1) => R.zero
      case (1, 0) => R.zero
      case (1, 1) => R.one
    }
  }

  def IsAtMostOne2(y: Var, x1: Var, x2: Var)(implicit R: u.semiring.Algebra) = ExplicitFactor.withConfigs(y, x1, x2){ conf =>
    (conf(y), conf(x1), conf(x2)) match {
      case (0, 0, 0) => R.one
      case (0, 0, 1) => R.zero
      case (0, 1, 0) => R.zero
      case (0, 1, 1) => R.zero
      case (1, 0, 0) => R.zero
      case (1, 0, 1) => R.one
      case (1, 1, 0) => R.one
      case (1, 1, 1) => R.zero
    }
  }

  //endregion

}
