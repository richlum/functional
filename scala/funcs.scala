def myreverse(lists:List[Any]) : List[Any] = myreversehelper(lists:List[Any],Nil)
def myreversehelper(lists:List[Any],result:List[Any]) : List[Any] = {
	if (lists == Nil) {
		result
	} else {
		myreversehelper(lists.tail, lists.head::result)
	}
}

def rev2(lists:List[Any]) : List[Any] = {
	var result = List[Any]()
	lists.foreach(e => result = e::result)
	result
}

def rev3(lists:List[Any]) : List[Any] = {
	lists.foldLeft(List[Any]()){
		(result,e) => e::result
	}
}


def geom(a:Int,r:Int,n:Int):Double ={
	if (n==1){
		a
	}else{
		a*Math.pow(r,(n-1)) + geom(a,r,n-1)
	}
}


def myrep (a:Any,b:Any,list:List[Any]): List[Any] = {
	if (list == Nil){
		Nil
	}else if (list.head == a) {
		b::myrep(a,b,list.tail)
	}else if (list.head == b) { 
		a::myrep(a,b,list.tail)
	}else{
		list.head::myrep(a,b,list.tail)
	}
}

