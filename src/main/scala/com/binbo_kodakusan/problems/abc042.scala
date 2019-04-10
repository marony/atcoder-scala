package com.binbo_kodakusan.abc042_a {
  // A - 和風いろはちゃんイージー / Iroha and Haiku (ABC Edition) https://atcoder.jp/contests/abc042/tasks/abc042_a

  import io.Source

  object Main extends App {
    Source.stdin.getLines().foreach { line =>
      val Array(w1, w2, w3) = line.split(' ').map(_.toInt)
      println(solve(w1, w2, w3))
    }

    def solve(w1: Int, w2: Int, w3: Int): String = {
      if (w1 == 5 && w2 == 5 && w3 == 7 ||
        w1 == 5 && w2 == 7 && w3 == 5 ||
        w1 == 7 && w2 == 5 && w3 == 5)
        "YES"
      else
        "NO"
    }
  }
}

package com.binbo_kodakusan.abc042_b {
  // B - 文字列大好きいろはちゃんイージー / Iroha Loves Strings (ABC Edition) https://atcoder.jp/contests/abc042/tasks/abc042_b

  import io.Source

  object Main extends App {
    val xs = Source.stdin.getLines()
    xs.next() // N, Lをスキップ

    println(solve(xs))

    def solve(xs: Iterator[String]): String = {
      xs.toSeq.sorted.foldLeft("")((acc, s) => acc + s)
    }
  }
}

package com.binbo_kodakusan.abc042_c1 {
  // C - こだわり者いろはちゃん / Iroha's Obsession https://atcoder.jp/contests/abc042/tasks/arc058_a

  import scala.io.Source

  // FIXME: なぜか"subtask1_X_07.txt"だけWA(Wrong Answer)
  object Main extends App {
    val xs = Source.stdin.getLines()
    val Array(n, k) = xs.next().split(' ').map(_.toInt)
    val ds = xs.next.split(' ').map(_.toInt)

    println(solve(n, ds))

    def solve(n: Int, ds: Seq[Int]): String = {
      def _loop(n: Int, l: Int, a: String): String = {
        if (l <= 0) {
          a
        } else {
          // 下1桁について
          val nn = n % 10
          if (ds.contains(nn)) {
            // 除外数字なのでインクリメントする
            // インクリメントしたら桁上がりするか
            val l1 = n.toString.length
            val l2 = (n + 1).toString.length
            _loop(n + 1, if (l1 != l2) l + 1 else l, a)
          } else {
            // 除外数字ではないので次の桁をチェックする
            _loop(n / 10, l - 1, nn.toString + a)
          }
        }
      }
      _loop(n, n.toString.length, "")
    }
  }
}

package com.binbo_kodakusan.abc042_c2 {
  // C - こだわり者いろはちゃん / Iroha's Obsession https://atcoder.jp/contests/abc042/tasks/arc058_a

  import scala.io.Source

  object Main extends App {
    val xs = Source.stdin.getLines()
    val Array(n, k) = xs.next().split(' ').map(_.toInt)
    val ds = xs.next.split(' ').map(_.toInt)

    println(solve(n, ds))

    def solve(n: Int, ds: Seq[Int]): String = {
      def check(n: Int): Boolean = {
        var f = true
        var nn = n
        while (nn > 0) {
          val nnn = nn % 10
          if (ds.contains(nnn))
            f = false
          nn = nn / 10
        }
        f
      }

      for (i <- n to 100000) {
        if (check(i)) {
          return i.toString
        }
      }
      ""
    }
  }
}

package com.binbo_kodakusan.abc042_test {

  import scala.util.Random

  object Main extends App {
    // A - 和風いろはちゃんイージー / Iroha and Haiku (ABC Edition) https://atcoder.jp/contests/abc042/tasks/abc042_a
    assert(com.binbo_kodakusan.abc042_a.Main.solve(5, 5, 7) == "YES")
    assert(com.binbo_kodakusan.abc042_a.Main.solve(7, 7, 5) == "NO")

    // B - 文字列大好きいろはちゃんイージー / Iroha Loves Strings (ABC Edition) https://atcoder.jp/contests/abc042/tasks/abc042_b
    {
      val xs = List(
        "dxx",
        "axx",
        "cxx",
      ).toIterator
      val a = com.binbo_kodakusan.abc042_b.Main.solve(xs)
//      println(s"a = $a")
      assert(a == "axxcxxdxx")
    }

    // C - こだわり者いろはちゃん / Iroha's Obsession https://atcoder.jp/contests/abc042/tasks/arc058_a
    assert(com.binbo_kodakusan.abc042_c2.Main.solve(1000, Array(1, 3, 4, 5, 6, 7, 8, 9)) == "2000")
    assert(com.binbo_kodakusan.abc042_c2.Main.solve(9999, Array(0)) == "9999")

    // abc042_c1, abc042_c2の違いは何かチェック
    // n = 3277, ds = Vector(4, 8, 2, 6, 1), a1 = 3377, a2 = 3300
    // n = 1450, ds = Vector(2, 1, 3, 8, 5), a1 = 4460, a2 = 4000
//    {
//      val rand = new Random()
//      while (true) {
//        val n = rand.nextInt(10000 - 1) + 1
//        val ds = rand.shuffle(0 to 9).take(rand.nextInt(9) + 1)
//        val a1 = com.binbo_kodakusan.abc042_c1.Main.solve(n, ds)
//        val a2 = com.binbo_kodakusan.abc042_c2.Main.solve(n, ds)
//        if (a1 != a2) {
//          println(s"n = $n, ds = $ds, a1 = $a1, a2 = $a2")
//          assert(false)
//        }
//      }
//    }
  }
}
