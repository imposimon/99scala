object main extends App {
	//P01 Find the last element of a list
	def last[T](list: List[T]): T = list match {
		case head :: Nil => head
		case _ :: tail => last(tail)
		case _ => throw new NoSuchElementException
	}

	//P02 Find the last but one element of a list
	def penultimate[T](list: List[T]): T = list match {
		case head :: _ :: Nil => head 
		case _ :: tail => penultimate(tail)
		case _ => throw new NoSuchElementException
	}
	
	//P03 Find the Kth element of a list 
	def nth[T](index: Int, list: List[T]): T = (index, list) match {
		case (0, head :: _) => head
		case (n, _ :: tail) => nth(n - 1, tail)
		case (_, Nil) => throw new ArrayIndexOutOfBoundsException
	}

	//P04 Find the number of elements of a list	
	def length[T](list: List[T]): Int = list match {
		case Nil => 0
		case _ :: tail => 1 + length(tail)
	}

	//P05 Reverse a list
	def reverse[T](list: List[T]): List[T] = list match {
		case Nil => Nil
		case head :: tail => reverse(tail) ::: List(head)
	}

	//test cases	
	val list1 = List(1, 2, 3)
	val list2 = List('a', 'b', 'c')
	val list3 = List(1)
	val emptyList = List()

	println(last(list1))
	println(last(list2))
	println(last(list3))

	println(penultimate(list1))
	println(penultimate(list2))
	//println(penultimate(list3))

	println(nth(1, list2))

	println(length(list1))
	println(length(list2))
	println(length(list3))
	println(length(emptyList))

	println(reverse(list2))
}
