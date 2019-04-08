package com.binbo_kodakusan.abc042_a {
  // A - 積雪深差 https://beta.atcoder.jp/contests/abc001/tasks/abc001_1

  import io.Source

  object Main extends App {
    val lines = Source.stdin.getLines().map(_.toInt).toArray
    val Array(h1, h2) = lines
    println(h1 - h2)
  }
}
