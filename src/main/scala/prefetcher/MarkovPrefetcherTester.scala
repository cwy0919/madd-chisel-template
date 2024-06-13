package prefetcher

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chisel3.simulator.EphemeralSimulator._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import scala.collection.mutable

// 打印访问历史和转移表的辅助函数
object PrintUtils {
  def printAccessHistory(dut: MarkovPrefetcher): Unit = {
    println("Access History:")
    for (i <- 0 until dut.historySize) {
      val entry = dut.io.accessHistory(i)
      val address = entry.address.peek().litValue
      val valid = entry.valid.peek().litToBoolean
      val prefetch = entry.prefetch.peek().litToBoolean
      val timestamp = entry.timestamp.peek().litValue
      if (valid) {
        println(s"  [$i] Address: $address, Prefetch: $prefetch, Timestamp: $timestamp")
      } else {
        println(s"  [$i] Invalid Entry")
      }
    }
    // TODO: 打印访问历史的逻辑
  }

  def printTransitionTable(dut: MarkovPrefetcher): Unit = {
    println("Transition Table:")
    for (i <- 0 until dut.numAddresses) {
      for (j <- 0 until dut.numAddresses) {
        val count = dut.io.transitionTable(i)(j).peek().litValue
        if (count > 0) {
          println(s"  From $i to $j: $count")
        }
      }
    }
    // TODO: 打印转移表的逻辑
  }

  def printDutOutputs(dut: MarkovPrefetcher, address: Int, cycle: Long, state: String): Unit = {
    println(s"[$cycle] State: $state, Address: $address")
    // 打印 DUT 输出的逻辑
    PrintUtils.printAccessHistory(dut)
    PrintUtils.printTransitionTable(dut)
  }
}

// 马尔科夫预取器的测试类
class MarkovPrefetcherSpec extends AnyFreeSpec with Matchers {
  // 运行测试的方法
  def runTest(dut: MarkovPrefetcher, addresses: Seq[Int], expectedEvents: Seq[MarkovPrefetcherSimulator.PrefetchEvent]): Unit = {
    var hits = 0
    var prefetchHits = 0
    var demandHits = 0
    var prefetchRequests = 0

    val fsmStateToString = Map(
      0 -> "Idle",
      1 -> "FindHit",
      2 -> "UpdateHistory1",
      3 -> "FindMostProbable",
      4 -> "UpdateHistory2",
      5 -> "ReportResult"
    ) // 有限状态机状态到字符串的映射

    // 迭代测试地址和预期事件
    for ((address, event) <- addresses.zip(expectedEvents)) {
      dut.io.address.poke(address.U) // 将地址输入DUT

      // 迭代FSM的每个步骤
      (0 to 5).foreach { step =>
        val cycle = dut.io.cycleCounter.peek().litValue // 获取当前周期数
        val fsmState = dut.io.fsmState.peek().litValue // 获取当前FSM状态
        val stateStr = fsmStateToString(fsmState.toInt) // 将FSM状态转换为字符串

        PrintUtils.printDutOutputs(dut, address, cycle.toLong, stateStr) // 打印DUT的输出

        // 在最后一步检查预期结果
        if (step == 5) {
          if (dut.io.hit.peek().litToBoolean) hits += 1
          if (dut.io.prefetchHit.peek().litToBoolean) prefetchHits += 1
          if (dut.io.demandHit.peek().litToBoolean) demandHits += 1
          if (dut.io.prefetch.peek().litToBoolean) prefetchRequests += 1

          dut.io.hit.expect(event.hit.B, s"Hit check failed for address $address. Expected: ${event.hit}, Actual: ${dut.io.hit.peek().litToBoolean}")
          dut.io.prefetchHit.expect(event.prefetchHit.B, s"Prefetch hit check failed for address $address. Expected: ${event.prefetchHit}, Actual: ${dut.io.prefetchHit.peek().litToBoolean}")
          dut.io.demandHit.expect(event.demandHit.B, s"Demand hit check failed for address $address. Expected: ${event.demandHit}, Actual: ${dut.io.demandHit.peek().litToBoolean}")
          dut.io.prefetch.expect(event.prefetch.B, s"Prefetch check failed for address $address. Expected: ${event.prefetch}, Actual: ${dut.io.prefetch.peek().litToBoolean}")
          if (event.prefetchAddress.isDefined) {
            dut.io.prefetchAddress.expect(event.prefetchAddress.get.U, s"Prefetch address check failed for address $address. Expected: ${event.prefetchAddress.get}, Actual: ${dut.io.prefetchAddress.peek().litValue}")
          }
        }

        dut.clock.step(1) // 时钟周期前进一步
      }
    }

    println(f"\nHits: $hits, Prefetch Hits: $prefetchHits, Demand Hits: $demandHits, Prefetch Requests: $prefetchRequests")
  }

  // 测试实例
  "MarkovPrefetcher should predict next address based on various patterns" - {
    val patterns = Seq(
       ("Sequential pattern", Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)), // 顺序模式
      ("Strided pattern", Seq(0, 2, 4, 6, 8, 10, 12, 14, 16, 18)), // 跨步模式
      ("Interleaved pattern", Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9).grouped(2).flatMap(_.reverse).toSeq), // 交错模式
      ("Random pattern", Seq.fill(10)(scala.util.Random.nextInt(32))), // 随机模式
      ("Repeated pattern", Seq(0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5)) // 重复模式
    )

    // 遍历每种模式进行测试
    patterns.foreach { case (patternName, addresses) =>
      println(s"\n$patternName:")
      println(s"\n  - ${addresses.mkString(", ")}")

      simulate(new MarkovPrefetcher()) { dut =>
        val expectedEvents = MarkovPrefetcherSimulator.simulatePrefetcher(32, addresses.toList)
        runTest(dut, addresses, expectedEvents) // 运行测试
      }
    }
  }
}

object MarkovPrefetcherTester extends App {
  (new MarkovPrefetcherSpec).execute()
}

object MarkovPrefetcherSimulator {

  // 定义预取事件类，包含地址、命中信息、预取地址和访问历史记录等字段
  case class PrefetchEvent(
    address: Int, // 访问的地址
    hit: Boolean, // 是否命中
    prefetchHit: Boolean, // 是否命中预取
    demandHit: Boolean, // 是否命中需求访问
    prefetch: Boolean, // 是否进行了预取
    prefetchAddress: Option[Int], // 预取的地址
    accessHistory: List[(Int, String)] // 访问历史记录
  )

  // 获取最可能的下一个地址
  def getMostProbableNextAddress(transitionTable: Array[Array[Int]], address: Int): Option[Int] = {
    // 找到当前地址行中转移次数最多的地址
    val maxTransitions = transitionTable(address).max
    // 如果最大转移次数为0，则返回None，否则返回对应的下一个地址
    if (maxTransitions == 0) None else Some(transitionTable(address).indexOf(maxTransitions))
  }

  // 模拟Markov预取器
  def simulatePrefetcher(numAddresses: Int, addresses: List[Int], historyWindowSize: Int = 5): List[PrefetchEvent] = {
    // 初始化转移表，每个元素初始值为0
    val transitionTable = Array.fill(numAddresses, numAddresses)(0)
    // 初始化访问历史记录队列，存储最近访问的地址及其访问类型
    val accessHistory = mutable.Queue[(Int, String)]()
    // 上一个访问的地址，初始为None
    var prevAddress: Option[Int] = None
    // 事件列表，存储每次访问的详细信息
    var events = List.empty[PrefetchEvent]

    // 遍历每个访问的地址
    addresses.foreach { address =>
      var hit = false // 是否命中
      var prefetchHit = false // 是否命中预取
      var demandHit = false // 是否命中需求访问
      var prefetch = false // 是否进行了预取
      var prefetchAddress: Option[Int] = None // 预取的地址

      // 检查当前地址是否在访问历史记录中
      accessHistory.zipWithIndex.foreach { case ((histAddress, accessType), i) =>
        // 如果当前访问地址在历史记录中找到
        if (address == histAddress) {
          hit = true // 表示命中
          // 如果命中且是预取类型，将其标记为非预取
          if (accessType == "Prefetch") {
            prefetchHit = true // 预取命中
            accessHistory(i) = (histAddress, "Demand") // 更新访问类型为需求
          } else {
            demandHit = true // 需求访问命中
          }
        }
      }

      // 如果未命中且有上一个地址，更新转移表
      if (!hit && prevAddress.isDefined) {
        transitionTable(prevAddress.get)(address) += 1 // 增加转移次数
      }

      // 更新访问历史记录，删除已经存在的地址
      accessHistory.dequeueAll(_._1 == address)
      // 将当前访问地址加入队列，标记为需求访问
      accessHistory.enqueue((address, "Demand"))

      // 如果访问历史记录超出窗口大小，移除最旧的记录
      if (accessHistory.size > historyWindowSize) {
        accessHistory.dequeue()
      }

      // 进行预取
      val predictedAddress = getMostProbableNextAddress(transitionTable, address)
      // 如果预测的地址存在且不在访问历史记录中，则进行预取
      if (predictedAddress.isDefined && !accessHistory.exists(_._1 == predictedAddress.get)) {
        // 将预测的地址加入访问历史记录队列，并标记为预取访问
        accessHistory.enqueue((predictedAddress.get, "Prefetch"))
        prefetch = true // 标记为进行了预取
        prefetchAddress = predictedAddress // 记录预取地址

        // 如果访问历史记录超出窗口大小，移除最旧的记录
        if (accessHistory.size > historyWindowSize) {
          accessHistory.dequeue()
        }
      }

      
      // 创建预取事件对象并加入事件列表
      events = events :+ PrefetchEvent(
        address, // 当前访问地址
        hit, // 是否命中
        prefetchHit, // 是否命中预取
        demandHit, // 是否命中需求访问
        prefetch, // 是否进行了预取
        prefetchAddress, // 预取的地址
        accessHistory.toList // 当前的访问历史记录
      )
      
      // 调试输出：
      println(s"Address: $address")
      println(s"  - Current Address: $address")
      println(s"  - Previous Address: ${prevAddress.getOrElse("None")}")
      println(s"  - Hit: $hit")
      println(s"  - Prefetch Hit: $prefetchHit")
      println(s"  - Demand Hit: $demandHit")
      println(s"  - Prefetch: $prefetch")
      println(s"  - Prefetch Address: ${prefetchAddress.getOrElse("None")}")
      printAccessHistory(accessHistory)
      printTransitionTable(transitionTable)

      // 更新前一个访问地址为当前地址
      prevAddress = Some(address)
    }

    events
  }

  // 打印访问历史记录
  def printAccessHistory(accessHistory: mutable.Queue[(Int, String)]): Unit = {
    val accessHistoryStr = accessHistory.map { case (address, accessType) =>
      s"($address, '$accessType')"
    }.mkString(", ")
    println(s"  - Access History: [$accessHistoryStr]")
  }

  // 打印转移表
  def printTransitionTable(transitionTable: Array[Array[Int]]): Unit = {
    val transitionTableStr = transitionTable.zipWithIndex.map { case (row, i) =>
      val entries = row.zipWithIndex.map { case (count, j) =>
        if (count > 0) s"$j($count)" else ""
      }.filter(_.nonEmpty).mkString(", ")
      if (entries.nonEmpty) s"$i -> [$entries]" else ""
    }.filter(_.nonEmpty).mkString(", ")
    println(s"  - Transition Table: $transitionTableStr")
  }
}