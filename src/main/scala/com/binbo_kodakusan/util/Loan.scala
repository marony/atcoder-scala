package com.binbo_kodakusan.util
import scala.language.reflectiveCalls

/**
  * Closableをusingで使う
  * ローンパターン
  */
object Loan {
  def using[T <: { def close() }](closable: T)(f: => Unit): Unit = {
    f
    closable.close()
  }
}

/**
  * ローンパターンで処理のログを出力
  * @param name
  */
class PerfMon(name: String = "") {
  private[this] val start = System.currentTimeMillis()
  println("========== " + name + " ==========")

  def close(): Unit = println("Elapsed(ms) : " + (System.currentTimeMillis() - start))
}
