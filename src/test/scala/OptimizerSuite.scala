import bfcompiler.common.{Location, Program}
import bfcompiler.intermediate.OperationType.{Increment, Write}
import bfcompiler.intermediate.{Operation, OperationType}
import bfcompiler.optimizer.Optimizer

class OptimizerSuite extends munit.FunSuite:
  private val optimizer     = Optimizer.default
  private val dummyLocation = Location.Local(0, 0)

  private def makeRepeatedOp[T <: OperationType](
      opTypeFactory: Int => T,
      n: Int
  ): List[Operation] =
    List.fill(n)(Operation(opTypeFactory(1), dummyLocation))

  private def testCollapse[T <: OperationType](
      opTypeFactory: (count: Int) => T,
      n: Int
  ): Unit =
    val ops       = makeRepeatedOp(opTypeFactory, n)
    val collapsed = optimizer.optimize(Program(ops))
    assertEquals(
      collapsed.ops,
      List(Operation(opTypeFactory(n), dummyLocation))
    )

  test("Optimizer collapses repeated increment operations correctly") {
    testCollapse(OperationType.Increment.apply, 1)
    testCollapse(OperationType.Increment.apply, 10)
  }

  test("Optimizer collapses repeated decrement operations correctly") {
    testCollapse(OperationType.Decrement.apply, 1)
    testCollapse(OperationType.Decrement.apply, 10)
  }

  test(
    "Optimizer collapses repeated increment data pointer operations correctly"
  ) {
    testCollapse(OperationType.IncrementDataPointer.apply, 1)
    testCollapse(OperationType.IncrementDataPointer.apply, 10)
  }

  test(
    "Optimizer collapses repeated decrement data pointer operations correctly"
  ) {
    testCollapse(OperationType.DecrementDataPointer.apply, 1)
    testCollapse(OperationType.DecrementDataPointer.apply, 10)
  }

  test("Optimizer does not affect non-collapsable ops") {
    val ops = List(
      Operation(OperationType.JumpBackwardNotEqualZero(0), dummyLocation),
      Operation(OperationType.JumpForwardEqualZero(0), dummyLocation),
      Operation(OperationType.Read, dummyLocation),
      Operation(OperationType.Write, dummyLocation)
    )
    val collapsed = optimizer.optimize(Program(ops)).ops
    assertEquals(collapsed, ops)
  }

  test("Optimizer collapses multiple groups of operations correctly") {
    val ops = makeRepeatedOp(OperationType.Increment.apply, 3) ++ List(
      Operation(OperationType.Write, dummyLocation)
    ) ++ makeRepeatedOp(OperationType.Decrement.apply, 7) ++ List(
      Operation(OperationType.Read, dummyLocation)
    )
    val collapsed = optimizer.optimize(Program(ops)).ops
    assertEquals(
      collapsed,
      List(
        Operation(OperationType.Increment(3), dummyLocation),
        Operation(OperationType.Write, dummyLocation),
        Operation(OperationType.Decrement(7), dummyLocation),
        Operation(OperationType.Read, dummyLocation)
      )
    )
  }
