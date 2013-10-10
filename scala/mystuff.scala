// scala to start REPL (commandline interface)
// :load fn.scala 
// >scalac Fn.scal  // to compile

//coersion
"abc".size
"abc"+4
4+"1.0"
// type conversion
"1.0".toFloat.toInt
10.toString

val a = 1 // immutable
var b = 3 // mutable
if (b<a) {
	println ("true")
} else {
	println ("false")
}

Nil // empty list only. Nil and numbers are not Booleans, cant test them as booleans

def whileLoop{
	var i = 2
	while (i <= 5){
		println(i)
		i+=1
	}
} //imperative style while loop


whileLoop // to call method

def forLoop(args:Array[String]) {
	println("Java style for loop")
	for (i <- 0 until args.length){
		println(args(i))
	}
}
//
forLoop (Array("testing","one","two","Three"))

//  functionname    comma sep list of parms           ret type
def functionName ( parm1:Int, parm2:Int, parm3:Int ): Int = {
	var sum:Int = 0
	sum = parm1 + parm2 + parm3
	return sum
}


functionName(1,2,3)  // Integer = 6


object adder{
	def addInt (a:Int, b:Int) : Int = {
		var sum:Int = 0
		sum = a+b
		return sum
	}
}

adder.addInt(2,3)

object Test{
	def main (args: Array[String]) {
		println("returned value: " + addInt (4,6))
		echoargs(args)  
		println("from main, args are: " + args.toList)
		args.foreach( println )
 
	}
	def addInt(a:Int,b:int) : Int = {
		var sum:Int = 0
		sum = a + b
		return sum
	}
	def echoargs(args: Array[String]){
		println("from echoargs")
		args.toList.foreach( println )
	}
}

Test

// arrays
var z:Array[String] = new Array[String](3)
z(0) = "Zebra"
z(2) = "Tango"
z(1) = "Omega"

z.foreach( println )

z = Array("Orange","Apple","Banana")
z.foreach(x => println (x))
for (fruit <- z) println (fruit) // equivalent to above
// create a List of outputs
val lengths = for (fruits <- z) yield fruits.length

println("Test.main(z)")
Test.main(z)


def rubyStyleForLoops(args:Array[String]) = {
	println("for loop using Ruby Style iteration")
	args.foreach{ arg => 
		println (arg)
	}
}

rubyStyleForLoops(z)


// ranges
val range = 0 to 10 by 2
val rang2 = 10 until 14
range.start
range.end
range.step
rang2.start
rang2.end
rang2.step

// tuples
val person = ("Richard", "Lum")
person._1
person._2
val (x,y) = (3,4)

//classes
class Person(fn:String, ln:String)
val gump = new Person("Forest","Gump")

class Point(val xc: Int, val yc: Int) {
   var x: Int = xc
   var y: Int = yc
   def move(dx: Int, dy: Int) {
      x = x + dx
      y = y + dy
      println ("Point x location : " + x);
      println ("Point y location : " + y);
   }
}

class Location(override val xc: Int, override val yc: Int,
   val zc :Int) extends Point(xc, yc){
   var z: Int = zc

   def move(dx: Int, dy: Int, dz: Int) {
//      x = x + dx
//      y = y + dy
      move(dx,dy)
      z = z + dz
      println ("Point x location : " + x);
      println ("Point y location : " + y);
      println ("Point z location : " + z);
   }
}


// by def object is a singleton class
object Test {
   def main(args: Array[String]) {
      val loc = new Location(10, 20, 15);

      // Move to a new location
      loc.move(10, 10, 5);
   }
}

Test.main(z)



//lists
List(1,2,3)
List ("One","Two","Three")
List (1,2,"Three")

var mylist = List(1,2,3,4)
mylist(0)
mylist(3)
mylist(4)


//sets
val animals = Set ("lions","Tigers","dogs", "bats")
animals + "armadillo"
animals - "Tigers"
animals ++ Set("armadillo","jellyfish")
animals -- Set("dogs","lions")
//animals & Set("lions","cucumbers","potatoes","bats") 
//// intersection doesnt seem to work on my REPL





