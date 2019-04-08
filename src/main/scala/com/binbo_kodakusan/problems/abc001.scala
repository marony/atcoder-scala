package com.binbo_kodakusan.abc001_a {
  // A - 積雪深差 https://beta.atcoder.jp/contests/abc001/tasks/abc001_1

  import io.Source

  object Main extends App {
    val xs = Source.stdin.getLines().map(_.toInt).toArray
    val ys = scala.collection.mutable.ListBuffer.empty[(Int, Int)]
    for (i <- 0 until xs.length) {
      if (i % 2 == 1) {
        ys += ((xs(i - 1), xs(i)))
      }
    }

    ys.foreach { case (h1, h2) =>
      println(solve(h1, h2))
    }

    def solve(h1: Int, h2: Int): String = {
      val a = h1 - h2
      a.toString
    }
  }
}

package com.binbo_kodakusan.abc001_b {
  // B - 視程の通報 https://beta.atcoder.jp/contests/abc001/tasks/abc001_2

  import io.Source

  object Main extends App {
    Source.stdin.getLines().map(_.toInt)
      .foreach(v => println(solve(v)))

    def solve(v: Int): String = {
      if (v < 100) {
        "00"
      } else if (v <= 5000) {
        f"${v * 10 / 1000}%02d"
      } else if (6000 <= v && v <= 30000) {
        ((v + 50000) / 1000).toString
      } else if (v <= 70000) {
        (((v - 30000) / 5 + 80000) / 1000).toString
      } else {
        "89"
      }
    }
  }
}

package com.binbo_kodakusan.abc001_c {
  // C - 風力観測 https://beta.atcoder.jp/contests/abc001/tasks/abc001_3

  import io.Source

  object Main extends App {
    Source.stdin.getLines().foreach { line =>
      val Array(deg, dis) = line.split(' ').map(_.toInt)
      println(solve(deg, dis))
    }

    def solve(deg: Int, dis: Int): String = {
      val d = distance(dis)
      val dir = degree(deg, d)
      s"$dir $d"
    }

    def degree(deg: Int, d: Int): String = {
      if (d == 0) {
        "C"
      } else {
        val a = Array(
          "N", "NNE", "NE", "ENE",
          "E", "ESE", "SE", "SSE",
          "S", "SSW", "SW", "WSW",
          "W", "WNW", "NW", "NNW",
          "N")
        a((deg * 10 + 1125) / 2250)
      }
    }

    def distance(dis: Int): Int = {
      if (dis < 15)
        0
      else if (dis < 93)
        1
      else if (dis < 201)
        2
      else if (dis < 327)
        3
      else if (dis < 477)
        4
      else if (dis < 645)
        5
      else if (dis < 831)
        6
      else if (dis < 1029)
        7
      else if (dis < 1245)
        8
      else if (dis < 1467)
        9
      else if (dis < 1707)
        10
      else if (dis < 1959)
        11
      else
        12
    }
  }
}

package com.binbo_kodakusan.abc001_d1 {
  // D - 感雨時刻の整理 https://atcoder.jp/contests/abc001/tasks/abc001_4

  import io.Source

  object Main extends App {
    val xs = Source.stdin.getLines().toSeq
    val n = xs.head
    val xss = xs.tail
      .map(s => {
        val a = s.split('-')
        (a(0).toInt, a(1).toInt)
      })

//    print(solve(xss))
    solve(xss)

    def solve(xs: Seq[(Int, Int)]): String = {
      var buf: Option[(Int, Int)] = None
      xs.sortBy(_._1).foreach { case (l, r) =>
        val ll = l / 5 * 5
        val rr = if (r % 5 == 0) r else r / 5 * 5 + 5
        buf match {
          case Some((l2, r2)) => {
            if (ll <= r2 && rr >= l2) {
              // 重複している
              val min = Array(ll, rr, l2, r2).min
              val max = Array(ll, rr, l2, r2).max
              buf = Some((min, max))
            } else {
              println("%04d".format(l2) + "-" + "%04d".format(r2))
              buf = Some((ll, rr))
            }
          }
          case _ => buf = Some((ll, rr))
        }
      }
      buf match {
        case Some((l2, r2)) => println("%04d".format(l2) + "-" + "%04d".format(r2))
      }
      ""
    }
  }
}

package com.binbo_kodakusan.abc001_d2 {
  // D - 感雨時刻の整理 https://atcoder.jp/contests/abc001/tasks/abc001_4

  import io.Source

  object Main extends App {
    val xs = Source.stdin.getLines()
    xs.next() // Nをスキップ
    val xss = xs
      .map(s => {
        val a = s.split('-')
        (a(0).toInt, a(1).toInt)
      })

    print(solve(xss))
//    solve(xss)

    def solve(xs: Iterator[(Int, Int)]): String = {
      val buf = Array.ofDim[Boolean](24 * 60 / 5 + 1)
      xs.foreach { case (l, r) =>
        val ll = ((l / 100 * 60) + (l % 100) / 5 * 5) / 5
        val rr = if (r % 5 == 0) ((r / 100 * 60) + (r % 100)) / 5 - 1
                      else ((r / 100 * 60) + (r % 100) / 5 * 5 + 5) / 5 - 1
        for (i <- ll to rr) {
          buf(i) = true
        }
      }
      var f = false
      for (i <- 0 until buf.length) {
        if (f) {
          if (!buf(i)) {
            val hh = i * 5 / 60
            val mm = (i * 5) % 60
            print("%02d".format(hh))
            println("%02d".format(mm))
            f = false
          }
        } else {
          if (buf(i)) {
            val hh = i * 5 / 60
            val mm = (i * 5) % 60
            print("%02d".format(hh))
            print("%02d".format(mm))
            print("-")
            f = true
          }
        }
      }
      ""
    }
  }
}

package com.binbo_kodakusan.abc001_test {
  object Main extends App {
    // A - 積雪深差 https://beta.atcoder.jp/contests/abc001/tasks/abc001_1
    assert(com.binbo_kodakusan.abc001_a.Main.solve(15, 10) == "5")
    assert(com.binbo_kodakusan.abc001_a.Main.solve(0, 0) == "0")
    assert(com.binbo_kodakusan.abc001_a.Main.solve(5, 20) == "-15")

    // B - 視程の通報 https://beta.atcoder.jp/contests/abc001/tasks/abc001_2
    assert(com.binbo_kodakusan.abc001_b.Main.solve(15000) == "65")
    assert(com.binbo_kodakusan.abc001_b.Main.solve(75000) == "89")
    assert(com.binbo_kodakusan.abc001_b.Main.solve(200) == "02")

    // C - 風力観測 https://beta.atcoder.jp/contests/abc001/tasks/abc001_3
    assert(com.binbo_kodakusan.abc001_c.Main.solve(2750, 628) == "W 5")
    assert(com.binbo_kodakusan.abc001_c.Main.solve(161, 8) == "C 0")
    assert(com.binbo_kodakusan.abc001_c.Main.solve(3263, 15) == "NNW 1")
    assert(com.binbo_kodakusan.abc001_c.Main.solve(1462, 1959) == "SE 12")
    assert(com.binbo_kodakusan.abc001_c.Main.solve(1687, 1029) == "SSE 8")
    assert(com.binbo_kodakusan.abc001_c.Main.solve(2587, 644) == "WSW 5")
    assert(com.binbo_kodakusan.abc001_c.Main.solve(113, 201) == "NNE 3")
    assert(com.binbo_kodakusan.abc001_c.Main.solve(2048, 16) == "SSW 1")

    // D - 感雨時刻の整理 https://atcoder.jp/contests/abc001/tasks/abc001_4
    {
      val xs = List(
        (1148, 1210),
        (1323, 1401),
        (1106, 1123),
        (1129, 1203),
      ).toIterator
      /*val a =*/ com.binbo_kodakusan.abc001_d2.Main.solve(xs)
//      println(s"a = $a")
//      assert(a == "1105-1210\n1320-1405\n")
    }
    {
      val xs = List(
        (0, 2400),
      ).toIterator
      /*val a =*/ com.binbo_kodakusan.abc001_d2.Main.solve(xs)
//      println(s"a = $a")
//      assert(a == "0000-2400\n")
    }
    {
      val xs = List(
        (1157, 1306),
        (1159, 1307),
        (1158, 1259),
        (1230, 1240),
        (1157, 1306),
        (1315, 1317),
      ).toIterator
      /*val a =*/ com.binbo_kodakusan.abc001_d2.Main.solve(xs)
//      println(s"a = $a")
//      assert(a == "1155-1310\n1315-1320\n")
    }
  }
}
