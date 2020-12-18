package xyz.hyperreal.dal
//import utest._
//
//object UnitTests extends TestSuite {
//
//  import Testing.eval
//
//  val tests = Tests {
//    test("add") {
//      assert(eval("1 + 2") == Num(3))
//      assert(eval("1 + 2.3") == Num(3.3))
//      assert(eval("1.2 + 2.3") == Num(3.5))
//      assert(eval("1000000000 + 2") == Num(1000000002))
//      assert(eval("10000000000 + 2") == Num(BigInt("10000000002")))
//      assert(eval("10000000000 + 2345.6") == Num(10000002345.6))
//      assert(eval("10000000000 + -9000000000") == Num(1000000000))
//    }
//    test("eq") {
//      assert(eval("3 = 3") == true)
//      assert(eval("3 = 4") == false)
//      assert(eval("3 = 3.0") == true)
//      assert(eval("3 = 3.1") == false)
//    }
//    test("div") {
//      assert(eval("3 div 12") == true)
//      assert(eval("3 div 10") == false)
//    }
//  }
//
//}
