
signature STREAM = 
  sig
    type 'T stream
    val map    : ('T -> 'R) -> 'T stream -> 'R stream
    val filter : ('T -> bool) -> 'T stream -> 'T stream
    val flatMap: ('T -> 'R stream) -> 'T stream -> 'R stream
    val fold   : ('A -> 'S -> 'A) -> 'S -> 'S stream -> 'A
  end

structure StreamImpl : STREAM =
 struct  
   datatype 'T stream = Stream of ('T -> bool) -> unit
   fun map f s = 
     let val (Stream streamf) = s
     in
	 let val iter = fn iterf => streamf (fn value => iterf (f value))
	 in
	     Stream iter
	 end
     end
 end
