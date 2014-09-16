

signature STREAM = 
  sig
    type 'T stream
    val map    : ('T -> 'R) -> 'T stream -> 'R stream
    val filter : ('T -> bool) -> 'T stream -> 'T stream
    val flatMap: ('T -> 'R stream) -> 'T stream -> 'R stream
    val fold   : ('A -> 'S -> 'A) -> 'S -> 'S stream -> 'A
    val ofArray : 'T array -> 'T stream
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
   fun filter pred s = raise Div
   fun flatMap f s = raise Div
   fun fold f a s = 
       let val (Stream streamf) = s
       in

       end
   fun ofArray arr = 
       let val gen = fn iterf =>
			let
			    val counter = ref 0
			    val size = Array.length arr
			in
			    while !counter < size do (
				iterf (Array.sub(arr, !counter));
				counter := !counter + 1
			    )
			end
       in
	   Stream gen
       end

 end
