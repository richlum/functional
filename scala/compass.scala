class Compass{
	// constructor
	val directions = List("north","east","south","west")
	var bearing = 0

	println("initial bearing: " )
	println(direction)

	def direction() = directions(bearing)

	def inform(turn: String){
		println ("turning " + turn + ". Bearing : " + direction)
	}

	def turnRight(){
		bearing = (bearing +1) % directions.size
		inform ("right")
	}

	def turnLeft(){
		bearing = (bearing + (directions.size -1)) % directions.size
		inform("left")
	}
}

var c = new Compass
c.turnRight
c.turnRight
c.turnRight
c.turnRight
c.turnLeft


class Person(val fn:String){
	println("Outer ctor")
	def this (fn:String, ln:String){
		this(fn)  // must call outer ctor first
		println("Inner ctor")
	}
	def talk(msg:String) = println(msg)
}

val bob = new Person("Bob")

val bobTate = new Person("Bob","Tate")
bobTate.talk("Hi!")

class Person2(val fn:String){
	def this (fn:String,ln:String){
		this(fn)
	}
	def talk(msg:String) = println(fn + " says " + msg)
	def id():String = fn
}
class Employee( override val fn: String, 
		val number: Int) extends Person2(fn){
	def this (fn:String, ln:String, number:Int){
		this(fn,number)
	}
	override def talk(message:String){
		println(fn + " " + " with number " + number + " says " + message)
	}
	override def id():String = number.toString
}

val employee = new Employee("Yoda",5432)
employee.talk("Learn Scala, we will.")

val emp2 = new Employee("Max","HeadRoom", 324)
employee.talk("wwwwwowwwww")

trait Nice {
	def greet() = println("Quack")
}

class Character(override val fn:String) extends Person(fn) with Nice

val flanders = new Character("Ned")
flanders.greet

