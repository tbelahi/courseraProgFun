package funsets

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {


  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }
  
  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   * 
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   * 
   *   val s1 = singletonSet(1)
   * 
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   * 
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   * 
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {
    
    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3". 
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(contains(s2, 2), "Singleton 2")
      assert(contains(s3, 3), "Singleton 3")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersection of singleton is null") {
    new TestSets {
      val s = intersect(s1, s2)
      assert(!contains(s, 1), "intersection 1")
      assert(!contains(s, 2), "intersection 2")
    }
  }
    

  test(" diff ") {
     new TestSets {
      val s = union(union(s1, s2), s3)
      assert(contains(diff(s, s1), 2), "diff 1")
      assert(contains(diff(s, s2), 1), "diff 2")
      assert(contains(diff(s, s2), 3), "diff 2")

    }
  }

  test("filter") {
    new TestSets {
      val s = union(union(s1, s2), s3)
      def p = (x: Int) => x==1
      assert(contains(filter(s,p), 1), "filter 1")
      assert(!contains(filter(s,p), 2), "filter 2")
      assert(!contains(filter(s,p), 3), "filter 3")
    }
  }


  test("tests forall function") {
    new TestSets {
      /*This is a test for which the claim is NOT true:*/
      val s = (x: Int) => x % 2 == 1
      val p = (x: Int) => x % 2 == 0

      assert(!forall(s, p), "s has at least one member for which p is NOT true")
    }
  }

  test("test exists function") {
    new TestSets {
      def s = (x: Int) => x % 2 == 0 
      def p = s2

      assert(exists(s,p), "s, the set of even number contains singleton 2")
      assert(!exists(s,s3), "s, the set of even number contains singleton 3")
    }
  }

  test("test map function") {
    new TestSets {
      val s = (x: Int) => (x <=1000 && x>=(-1000))
      def f(x: Int): Int = x*x
      val p = map(s, f)
      assert(contains(p, 10000), "test map 1")
      assert(!contains(p, -100), "test map 2")
    }
  }
  
   
}
