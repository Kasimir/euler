package de.euler.task33

class Task33{
	case class Frac(val n:Int, val z:Int){
		def eq(o:Frac):Boolean = {
			val nt = wide(o.z)
			val no = o.wide(z)
			return nt == no 
		}
		def wide(i:Int):Frac = {
			return Frac(n*i,z*i)
		}
		def uo(f:Frac): List[Frac] = {
			var res = List()
			if
		}
	}


	def solve():Any = {
		def toI(l:List[Int])={
			if(l==Nil) return 0
			return l.head + 10*toI(l.tail)
		}
		for(n1 <- 1 to 9;
			n2 <- 0 to 9;
			z1 <- 1 to 9;
			z2 <- 0 to 9;
			
	}
}
