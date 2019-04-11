package com.binbo_kodakusan.abc043_a {
  // A - キャンディーとN人の子供イージー / Children and Candies (ABC Edit) https://atcoder.jp/contests/abc043/tasks/abc043_a

  import io.Source

  object Main extends App {
    Source.stdin.getLines().foreach { line =>
      val n = line.toInt
      println(solve(n))
    }

    def solve(n: Int): String = {
      ((1 + n) * n / 2).toString
    }
  }
}

package com.binbo_kodakusan.abc043_b {
  // B - バイナリハックイージー / Unhappy Hacking (ABC Edit) https://atcoder.jp/contests/abc043/tasks/abc043_b

  import io.Source

  object Main extends App {
    Source.stdin.getLines().foreach(l => println(solve(l)))

    def solve(s: String): String = {
      var a = ""
      s.foreach { c =>
        c match {
          case 'B' => a = if (a.length > 0) a.substring(0, a.length - 1) else a
          case ch => a += ch
        }
      }
      a
    }
  }
}

package com.binbo_kodakusan.abc043_c1 {
  // C - いっしょ / Be Together https://atcoder.jp/contests/abc043/tasks/arc059_a

  import io.Source

  object Main extends App {
    val xs = Source.stdin.getLines()
    val n = xs.next().toInt
    val ns = xs.next().split(' ').map(_.toInt)

    println(solve(ns))

    def solve(ns: Seq[Int]): String = {
      // 平均かその前後の値に書き換えるのが一番いいはず
      val nss = ns.sorted
      val min = nss.min
      val max = nss.max
      // 平均で計算
      val a = (min + max) / 2
      var ans = cost(a, nss)
      // 1つ前の値で計算
      val pi = nss.lastIndexWhere(_ < a)
      if (pi >= 0) {
        ans = Math.min(cost(ns(pi), nss), ans)
      }
      // 1つ後の値で計算
      val ni = nss.indexWhere(_ > a)
      if (ni >= 0) {
        ans = Math.min(cost(ns(ni), nss), ans)
      }
      ans.toString
    }

    def cost(n: Int, ns: Seq[Int]): Int = {
      ns.map(m => Math.pow(m - n, 2).toInt).sum
    }
  }
}

package com.binbo_kodakusan.abc043_c2 {
  // C - いっしょ / Be Together https://atcoder.jp/contests/abc043/tasks/arc059_a

  import io.Source

  object Main extends App {
    val xs = Source.stdin.getLines()
    val n = xs.next().toInt
    val ns = xs.next().split(' ').map(_.toInt)

    println(solve(ns))

    def solve(ns: Seq[Int]): String = {
      // 総当り
      var ans = Int.MaxValue
      (-100 to 100).foreach { n =>
        val a = cost(n, ns)
        ans = Math.min(a, ans)
      }
      ans.toString
    }

    def cost(n: Int, ns: Seq[Int]): Int = {
      ns.map(m => Math.pow(m - n, 2).toInt).sum
    }
  }
}

package com.binbo_kodakusan.abc043_d {
  // D - アンバランス / Unbalanced https://atcoder.jp/contests/abc043/tasks/arc059_b

  import io.Source
  import scala.collection.mutable

  object Main extends App {
    Source.stdin.getLines().foreach { s =>
      println(solve(s))
    }

    def solve(s: String): String = {
      // 総当り
      (1 to s.length - 1).foreach { i =>
        (i + 1 to s.length).foreach { j =>
          val ss = s.substring(i - 1, j)
//          println(s"ss = $ss, i = $i, j = $j")
          if (check(ss)) {
            return s"$i $j"
          }
        }
      }
      "-1 -1"
    }

    def check(s: String): Boolean = {
      val l = s.length
      val map = mutable.Map.empty[Char, Int]
      s.foreach { c =>
        if (map.contains(c)) {
          val cc = map(c)
//          println(s"s = $s, cc = ${cc + 1}, c = $c")
          if (cc + 1 > l / 2)
            return true
          map(c) = cc + 1
        } else {
          map += (c -> 1)
        }
      }
      false
    }
  }
}

package com.binbo_kodakusan.abc043_test {

  object Main extends App {
    // A - キャンディーとN人の子供イージー / Children and Candies (ABC Edit) https://atcoder.jp/contests/abc043/tasks/abc043_a
    assert(com.binbo_kodakusan.abc043_a.Main.solve(3) == "6")
    assert(com.binbo_kodakusan.abc043_a.Main.solve(10) == "55")
    assert(com.binbo_kodakusan.abc043_a.Main.solve(1) == "1")
    // B - バイナリハックイージー / Unhappy Hacking (ABC Edit) https://atcoder.jp/contests/abc043/tasks/abc043_b
    assert(com.binbo_kodakusan.abc043_b.Main.solve("01B0") == "00")
    assert(com.binbo_kodakusan.abc043_b.Main.solve("0BB1") == "1")
    // C - いっしょ / Be Together https://atcoder.jp/contests/abc043/tasks/arc059_a
    println(com.binbo_kodakusan.abc043_c1.Main.solve(Array(1, 100, 20)))
    println(com.binbo_kodakusan.abc043_c2.Main.solve(Array(1, 100, 20)))

    assert(com.binbo_kodakusan.abc043_c2.Main.solve(Array(4, 8)) == "8")
    assert(com.binbo_kodakusan.abc043_c2.Main.solve(Array(1, 1, 3)) == "3")
    assert(com.binbo_kodakusan.abc043_c2.Main.solve(Array(4, 2, 5)) == "5")
    assert(com.binbo_kodakusan.abc043_c2.Main.solve(Array(-100, -100, -100, -100)) == "0")

    // D - アンバランス / Unbalanced https://atcoder.jp/contests/abc043/tasks/arc059_b
    // FIXME: 大きな文字列はTLE
    assert(com.binbo_kodakusan.abc043_d.Main.solve("needed") == "2 5")
    assert(com.binbo_kodakusan.abc043_d.Main.solve("atcoder") == "-1 -1")
  }
}