

signature STREAM = 
  sig
    type 't stream
    val map    : ('t -> 'r) -> 't stream -> 'r stream
    val filter : ('t -> bool) -> 't stream -> 't stream
    val flatMap: ('t -> 'r stream) -> 't stream -> 'r stream
    val fold   : ('a -> 't -> 'a) -> 'a -> 't stream -> 'a
    val ofArray : 't array -> 't stream
    val length : 't stream -> int
    val sum :  int stream -> int
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
   fun filter pred s = 
       let val (Stream streamf) = s
       in
	   let val iter = fn iterf => streamf(fn value => if pred(value) = true then iterf(value) else true)
	   in
	       Stream iter
	   end
       end
   fun flatMap f s = raise Div
   fun fold f a s = 
       let val (Stream streamf) = s
	   and x = ref a
       in
	   streamf (fn value => (x := f (!x) value; true));
	   !x
       end
   fun length s = fold (fn a => fn _ => a + 1) 0 s
   fun sum s = fold (fn a => fn ss => a + ss) 0 s
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

fun lengthBaseline arr = 
    let
	val counter = ref 0
	val length = ref 0
	val size = Array.length arr
    in
	while !counter < size do (
	    let 
		val item = Array.sub(arr, !counter)
	    in
		length := (if item > 10 andalso item > 11 andalso item > 12 andalso item > 13 andalso item > 14 
			      andalso item > 15 andalso item > 16 andalso item > 17 then
			      !length +1 
			  else
			      !length)
	    end;
	    counter := !counter + 1
	);
	!length
    end

fun measure (s,f) =
let
    val timer = ref (Timer.startRealTimer ());
    val ret = f()
    val time = Timer.checkRealTimer(!timer)
in
    print (s ^ " time: " ^ Time.toString(time) ^ " sec/op\n");
    ret
end

(* Test Program *)
structure Program = struct

fun main () =
    let
	val _ = print ("Running\n");
	val backingArr = Array.tabulate (600000000, fn i => i);
	val v = Stream.ofArray(backingArr);
	fun length values = Stream.length values;
        fun length' values = ((Stream.fold (fn a => fn s => a + s) 0) o (Stream.map(fn x => x * x))) values
	fun length'' values = (Stream.length o Stream.filter(fn v => v > 10) 
			                     o Stream.filter(fn v => v > 11)
					     o Stream.filter(fn v => v > 12)
					     o Stream.filter(fn v => v > 13)
					     o Stream.filter(fn v => v > 14)
					     o Stream.filter(fn v => v > 15)
					     o Stream.filter(fn v => v > 16)
					     o Stream.filter(fn v => v > 17)) values
    in
	let 
	    val lengthRet = measure("Streams", fn _ => length''(v));
	    val lengthBaselineRet = measure ("Baseline", fn _ => lengthBaseline(backingArr))
	in
	    print ("Streams  Length = " ^ Int.toString(lengthRet) ^ "\n");
	    print ("Baseline Length = " ^ Int.toString(lengthBaselineRet) ^ "\n")
	end
    end
end

val _ = Program.main ()
