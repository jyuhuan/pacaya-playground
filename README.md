## Pacaya Playground

__Pacaya Playground__ provides usage for Scala users of the [Pacaya](https://github.com/mgormley/pacaya) library. The classes provided are wrappers of those having the same names in Pacaya, yet providing methods that seem native by Scala users. A Scala user can now use Pacaya with an intuitive, declarative syntax.

### Example

Consider the following factor graph

![](http://yuhuan.me/resource/img/pacaya-playground-example.png)

where
- All variables are Boolean.
- φ<sub>Choose1</sub>(X, X<sub>1</sub>, X<sub>2</sub>) is a factor that connects X and X<sub>1</sub> and X<sub>2</sub>. It puts a hard constraint that when X is true, exactly one of X<sub>1</sub> and X<sub>2</sub> should be true, and if X is false, neither of X<sub>1</sub> or X<sub>2</sub> can be true.   
  φ<sub>Choose1</sub>(Y, Y<sub>1</sub>, Y<sub>2</sub>) is similar. 
- φ<sub>Equal</sub>(X, Y) forces X and Y to have the same value.
- Unary factors control the local opinions of individual variables.

To create this factor graph, define variables as follows

```scala
val x = BooleanVar("vS")
val x1 = BooleanVar("vPos")
val x2 = BooleanVar("vNeg")

val y = BooleanVar("vA")
val y1 = BooleanVar("vAgt")
val y2 = BooleanVar("vThm")
```

Then, define the factor graph:

```scala
val fg = FactorGraph(
  Factor(x1)(R.one, R.fromReal(2)),
  Factor(x2)(R.one, R.fromReal(0.5)),
  TwoChooseOne(x, x1, x2),
  Factor(y1)(R.one, R.fromReal(0.5)),
  Factor(y2)(R.one, R.fromReal(2.0)),
  TwoChooseOne(y, y1, y2),
  Equal(x, y)
)
```
