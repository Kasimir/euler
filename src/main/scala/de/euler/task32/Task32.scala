package de.euler.task32

import scala.collection.BitSet

class Task32{

	def ti(ol:List[Int]):Int = {
		def iti(l:List[Int]):Int = {
			if(l == Nil) return 0
			return l.head + 10 * iti(l.tail)
		}
		return iti(ol.reverse)
	}

	def tl(i:Int):List[Int] = {
		return i.toString.toSeq.map(_.asDigit).toList
	}

	def isPandigital(l:List[Int]):Boolean = {
		if(l.size != 9) return false
		val digits = BitSet() ++ (1 to 9)
		val oDigits = BitSet() ++ l
		return digits == oDigits
	}
	def solve:Any = {
		val digits = BitSet() ++ (1 to 9)
		val pandigitals = (for(f1 <- digits;
			r1 = digits - f1;
			f2 <- r1;
			r2 = r1 - f2;
			s1 <- r2;
			r3 = r2 - s1;
			s2 <- r3;
			r4 = r3 - s2;
			s3 <- r4;
			r5 = r4 - s3;
			first = ti(List(f1,f2));
			second = ti(List(s1,s2,s3));
			prod = first*second;
			if(isPandigital(tl(first) ::: tl(second) ::: tl(prod)))
		) yield (first,second,prod)).toList
		pandigitals.foreach(println)
		val pan2 = (for(f <- 1 to 9;
			s <- 1000 to 9999;
			p = f*s;
			if(isPandigital(tl(f) ::: tl(s) ::: tl(p)))) yield(f,s,p)).toList	
		pan2.foreach(println)
		return (BitSet() ++ pandigitals.map(_._3) ++ pan2.map(_._3)).sum
	}
}
