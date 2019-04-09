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

package com.binbo_kodakusan.abc042_c {
  // C - こだわり者いろはちゃん / Iroha's Obsession https://atcoder.jp/contests/abc042/tasks/arc058_a

  import io.Source

  object Main extends App {
    val xs = Source.stdin.getLines()
    val Array(n, k/*, _**/) = xs.next().split(' ').map(_.toInt)
    val ds = xs.next().split(' ').map(_.toInt)

    println(solve(n, ds))

    def solve(n: Int, ds: Seq[Int]): String = {
      ""
    }
  }
}

package com.binbo_kodakusan.abc042_test {
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
      println(s"a = $a")
      assert(a == "axxcxxdxx")
    }

    // C - こだわり者いろはちゃん / Iroha's Obsession https://atcoder.jp/contests/abc042/tasks/arc058_a
  }
}
