(* Stream Library *)

signature STREAM = sig
    type 't stream
    val map    : ('t -> 'r) -> 't stream -> 'r stream
    val filter : ('t -> bool) -> 't stream -> 't stream
    val takeWhile : ('t -> bool) -> 't stream -> 't stream
    val skip : int -> 't stream -> 't stream
    val flatMap: ('t -> 'r stream) -> 't stream -> 'r stream
    val fold   : ('a -> 't -> 'a) -> 'a -> 't stream -> 'a
    val ofArray : 't array -> 't stream
    val length : 't stream -> int
    val sum :  LargeInt.int stream -> LargeInt.int
end

structure Stream : STREAM = struct 
  datatype 't stream = Stream of ('t -> bool) -> unit

  fun map f (Stream streamf) = 
      Stream (fn iterf => streamf (fn value => iterf (f value)))
	  
  fun filter pred (Stream streamf) = 
      Stream (fn iterf => 
		 streamf(fn value => if pred(value) = true then iterf(value) else true))
	  
  fun flatMap f (Stream outerf) = 
      Stream (fn iterf => 
		 outerf(fn value => 
			   let val (Stream innerf) = f(value)
			   in
			       innerf(iterf);
			       true
			   end))
	  
  fun fold f a (Stream streamf) = 
      let val x = ref a
      in
	  streamf (fn value => (x := f (!x) value; true));
	  !x
      end
	  
  fun length s = fold (fn a => fn _ => a + 1) 0 s

  fun sum s = fold (fn a => fn ss => a + ss) (LargeInt.fromInt 0) s

  fun filter pred (Stream streamf) = 
      Stream (fn iterf => 
		 streamf(fn value => if pred(value) = true 
				     then iterf(value) 
				     else true))

  fun takeWhile pred (Stream streamf) = 
      Stream (fn iterf => 
		 streamf(fn value => if pred(value) = true 
				     then iterf(value) 
				     else false))

  fun skip n (Stream streamf) = 
      let val count = ref 0
	  val iter = fn iterf => 
			streamf(fn value => 
				   let val _ = count := !count+1
				   in
				       if !count > n
				       then iterf(value) 
				       else true
				   end)
      in
	  Stream iter
      end

  fun ofArray arr = 
      let val gen = 
	   fn iterf =>
	      let
		  val counter = ref 0
		  val cont = ref true
		  val size = Array.length arr
	      in
		  while !counter < size andalso !cont do (
		      cont := iterf (Array.sub(arr, !counter));
		      counter := !counter + 1
		  )
	      end
      in
	  Stream gen
      end
end

(* Baseline functions *)

fun filters_6_baseline arr = 
    let
	val counter = ref 0
	val length = ref 0
	val size = Array.length arr
    in
	while !counter < size do (
	    let 
		val item = Array.sub(arr, !counter)
	    in
		length := (if item > 10 andalso item > 11 andalso item > 12 andalso 
			      item > 13 andalso item > 14 andalso item > 15
			   then
			       !length +1 
			   else
			       !length)
	    end;
	    counter := !counter + 1
	);
	!length
    end

fun cartBaseline arr1 arr2 = 
    let
	val counter1 = ref 0
	val counter2 = ref 0
	val sum = ref (LargeInt.fromInt 0)
	val size1 = Array.length arr1
	val size2 = Array.length arr2
    in
	while !counter1 < size1 do (
	    let 
		val item1 = Array.sub(arr1, !counter1);
	    in
		while !counter2 < size2 do (
		    let val item2 = Array.sub(arr2, !counter2)
		    in
			sum := !sum + item1*item2
		    end;
		    counter2 := !counter2 + 1
		)
	    end;
	    counter2 := 0;
	    counter1 := !counter1 + 1
	);
	!sum
    end

fun measure (s, f) =
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

	(* Backing Arrays *)
	val backingArr = Array.tabulate (3000000, fn i => i);
	val backingArrCart1 = Array.tabulate (100, fn i => Int.toLarge i);
	val backingArrCart2 = Array.tabulate (10, fn i => Int.toLarge i);

	(* Stream wrapping *)
	val v = Stream.ofArray backingArr;
	val v1 = Stream.ofArray backingArrCart1;
	val v2 = Stream.ofArray backingArrCart2;

	(* Stream functions *)
	fun length values = Stream.length values;
	fun map_fold values = ((Stream.fold (fn a => fn s => a + s) 0) 
			       o (Stream.map(fn x => x * x))) values;
	fun filters_6 values = (Stream.length 
				o Stream.filter(fn v => v > 10) 
				o Stream.filter(fn v => v > 11)
				o Stream.filter(fn v => v > 12)
				o Stream.filter(fn v => v > 13)
				o Stream.filter(fn v => v > 14)
				o Stream.filter(fn v => v > 15)) values;
	fun cart v1' v2' = (Stream.sum o Stream.flatMap(fn x => Stream.map (fn y => x * y) v2')) v1'
    in
	let 
	    (* Benchmark Execution *)
	    val lengthRet = measure("Streams filters_6", fn _ => filters_6 v);
	    val lengthBaselineRet = measure ("Baseline filters_6", fn _ => filters_6_baseline backingArr);
	    val cartRet = measure("Streams cart", fn _ => cart v1 v2);
	    val cartBaselineRet  = measure ("Baseline cart", fn _ => cartBaseline backingArrCart1 backingArrCart2)
	in
	    print ("\nValidation\n");
	    print ("Streams  filters_6 = " ^ Int.toString(lengthRet) ^ "\n");
	    print ("Baseline filters_6 = " ^ Int.toString(lengthBaselineRet) ^ "\n");
	    print ("Streams  cart     = " ^ LargeInt.toString(cartRet) ^ "\n");
	    print ("Baseline cart     = " ^ LargeInt.toString(cartBaselineRet) ^ "\n")
	end
    end
end

val _ = Program.main ()
