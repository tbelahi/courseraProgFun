package funsets

object Main extends App {
  import FunSets._
  println(contains(singletonSet(1), 1))
  println(contains(union(singletonSet(1),singletonSet(2)), 1))
  println(contains(intersect(singletonSet(1),singletonSet(2)), 1))
  println(contains(diff(union(singletonSet(1),singletonSet(2)), singletonSet(1)), 2))
  println(contains(filter(union(singletonSet(1),singletonSet(2)), (x => x==1)), 1))
  val s = (x: Int) => (x <=10 && x>=(-10))
  def f(x: Int): Int = x*x
  val p = map(s, f)
  printSet(p)
}
