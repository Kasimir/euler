package de.euler.task24

class Task24 {
  //method to find the next permutation
  //find element l, after wich all following elements are in descending order
  //pick smallest element in descending order, larger than l and bring it to pos (l)
  //sort elements after pos(l) in ascending order

  /*
   *(1,2,3) ((1),2,(3)) -> (x._1 ::: tn =( x._3.filter(_>x._2 ).min :: (x._2 :: (x._3.without(tn))).sort))
   *(1,3,2) (Nil,1,(2,3)) -> (x._3 (min > pivot)))
   *
  */

  def nextPerm(rList: List[Int]): List[Int] = {
    /*
     * takes a permutation and pivotizes it
     * rList: a reversed list of the actual permutation eg. (3,1,2) for the permutation (2,1,3)
     * returns:a pivotized list ((3),1,(2))
     */
    def pivotize(rList: List[Int]): (List[Int], Int, List[Int]) = {
      if (rList.tail == Nil) { 
        return (Nil, rList.head, Nil)
      } else if (rList.head > rList.tail.head) {
        return (List(rList.head), rList.tail.head, rList.tail.tail)
      } else {
        val tailResult = pivotize(rList.tail)
        return (rList.head :: tailResult._1, tailResult._2, tailResult._3)
      }
    }
    val piv = pivotize(rList)
    val newpiv2list = (piv._1.filter(_ > piv._2))
    if(newpiv2list == Nil){
      return Nil
    }
    val newpiv2 = newpiv2list.min
    val newpiv3 = (piv._2 :: piv._1.filter(_ != newpiv2)).sorted.reverse
    val result =  newpiv3  ::: (newpiv2 :: piv._3)
    return result
  }

  def permStream(elements: Int): Stream[List[Int]] = {
    val initList = (0 to (elements-1)).toList.reverse
    Stream.iterate(initList)(nextPerm).takeWhile(_ != Nil)
  }

  def solve(elements:Int = 10,nthPerm:Int = 1000000): Any = {
    val perms = for(perm <- permStream(elements).take(nthPerm)) yield perm
    return perms.last.reverse
  }

  def listAll(n:Int) = {
    for(perm <- permStream(n)) println(perm.reverse)
  }

}
