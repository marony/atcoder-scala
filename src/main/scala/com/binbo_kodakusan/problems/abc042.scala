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

package com.binbo_kodakusan.abc042_d1 {
  // D - いろはちゃんとマス目 / Iroha and a Grid https://atcoder.jp/contests/abc042/tasks/arc058_b

  import scala.io.Source

  object Main extends App {
    val xs = Source.stdin.getLines()
    val Array(h, w, a, b) = xs.next().split(' ').map(_.toInt)

    println(solve(h, w, a, b))

    def solve(h: Int, w: Int, a: Int, b: Int): String = {
      // a, bを気にせずに全探索してみる
      var xs = List((0, 0))
      var c = 0
      while (true) {
        if (xs.length <= 0)
          return c.toString
        val (x, y) = xs.head
        xs = xs.tail
        if (x < w - 1)
          xs = (x + 1, y) +: xs
        else if (y == h - 1)
          c += 1
        if (y < h - 1)
          xs = (x, y + 1) +: xs
        else if (x == w - 1)
          c += 1
//        println(s"($x, $y), $c")
      }
      ""
    }
  }
}

package com.binbo_kodakusan.abc042_d2 {
  // D - いろはちゃんとマス目 / Iroha and a Grid https://atcoder.jp/contests/abc042/tasks/arc058_b

  import scala.io.Source
  import scala.collection.mutable

  object Main extends App {
    val xs = Source.stdin.getLines()
    val Array(h, w, a, b) = xs.next().split(' ').map(_.toInt)

    var cache: mutable.Map[(BigInt, BigInt), BigInt] = null
    println(solve(h, w, a, b))

    def tri(h: BigInt, w: BigInt): BigInt = {
      if (h == 1)
        1
      else {
        if (cache.contains((h, w))) {
          cache((h, w))
        } else {
          val a = (BigInt(1) to w)
            .foldLeft(BigInt(0))((acc, n) => acc + tri(h - BigInt(1), n))
          cache += ((h, w) -> a)
          a
        }
      }
    }

    def solve(h: Int, w: Int, a: Int, b: Int): String = {
      cache = mutable.Map.empty[(BigInt, BigInt), BigInt]
      tri(h, w).toString
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
    // D - いろはちゃんとマス目 / Iroha and a Grid https://atcoder.jp/contests/abc042/tasks/arc058_b
    // FIXME: 解けない
    {
      val h = 2
      val w = 3
      val a = 1
      val b = 1
      val ans = com.binbo_kodakusan.abc042_d2.Main.solve(h, w, a, b)
      println(s"2: h = $h, w = $w, a = $a, b = $b, ans = $ans")
    }
    {
      val h = 10
      val w = 7
      val a = 3
      val b = 4
      val ans = com.binbo_kodakusan.abc042_d2.Main.solve(h, w, a, b)
      println(s"2: h = $h, w = $w, a = $a, b = $b, ans = $ans")
      val ans2 = com.binbo_kodakusan.abc042_d2.Main.solve(a, b, a, b)
      println(s"2: h = $h, w = $w, a = $a, b = $b, ans2 = $ans2")
      val ans3 = com.binbo_kodakusan.abc042_d2.Main.solve(h - a, w - b, a, b)
      println(s"2: h = $h, w = $w, a = $a, b = $b, ans3 = $ans3")
    }
//    {
//      val h = 100000
//      val w = 100000
//      val a = 99999
//      val b = 99999
//      val ans = com.binbo_kodakusan.abc042_d2.Main.solve(h, w, a, b)
//      println(s"2: h = $h, w = $w, a = $a, b = $b, ans = $ans")
//    }
//    {
//      val h = 100000
//      val w = 100000
//      val a = 44444
//      val b = 55555
//      val ans = com.binbo_kodakusan.abc042_d2.Main.solve(h, w, a, b)
//      println(s"2: h = $h, w = $w, a = $a, b = $b, ans = $ans")
//    }
  }
}
