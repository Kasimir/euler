package de.euler.task9

class Task9{
  def solve():Long = {
    val result = for(a <- 1 to 998; b <- 1 to (999-a); c = 1000 - a - b; if(a*a + b*b == c*c)) yield Seq(a,b,c)
    println(result)
    return result.head.product
  }
}