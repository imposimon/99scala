object main extends App {
	def last[T](list: List[T]): T = list match {
		case head :: Nil => head
		case _ :: tail => last(tail)
		case _ => throw new NoSuchElementException
	}

	def penultimate[T](list: List[T]): T = list match {
		case head :: _ :: Nil => head 
		case _ :: tail => penultimate(tail)
		case _ => throw new NoSuchElementException
	}

	def nth[T](index: Int, list: List[T]): T = (index, list) match {
		case (0, head :: _) => head
		case (n, _ :: tail) => nth(n - 1, tail)
		case (_, Nil) => throw new ArrayIndexOutOfBoundsException
	}

	val list1 = List(1, 2, 3)
	val list2 = List('a', 'b', 'c')
	val list3 = List(1)
	val emptyList = List()

	println(last(list1))
	println(last(list2))
	println(last(list3))
	println(penultimate(list1))
	println(penultimate(list2))
	println(penultimate(list3))
	println(nth(1, list2))
}