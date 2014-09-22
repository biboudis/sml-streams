

signature STREAM = 
  sig
    type 't stream
    val map    : ('t -> 'r) -> 't stream -> 'r stream
    val filter : ('t -> bool) -> 't stream -> 't stream
    val flatMap: ('t -> 'r stream) -> 't stream -> 'r stream
    val fold   : ('a -> 't -> 'a) -> 'a -> 't stream -> 'a
    val ofArray : 't array -> 't stream
  end

structure Stream : STREAM =
 struct
   datatype 't stream = Stream of ('t -> bool) -> unit
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
	   and x = ref a
       in
	   streamf (fn value => (x := f (!x) value; true));
	   !x
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

(* Test *)
structure Test =
struct

fun main (prog_name, args) =
    let
	val _ = print ("Running: " ^ prog_name ^ "\n");
	val v = Stream.ofArray(Array.array(1000,1));
	fun sumEven values = ((Stream.fold (fn a => fn s => a + s) 0) o (Stream.filter (fn i => (i mod 2) = 0))) values
    in
	print "fold";
	Stream.fold (fn a => fn s => a + s) 0 v;
	print "foldWhere";
	sumEven v
    end
end
