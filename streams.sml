(* Aggelos Biboudis, Nick Palladinos, Henry Cejtin  *)
fun recur (z: 'a, f: 'a * ('a -> 'b) -> 'b): 'b =
    let fun loop z = f (z, loop)
    in loop z
    end

(* datatype 't seq = Empty  *)
(* 		| Cons of 'a * (unit -> 'a seq) *)
				  
signature STREAM = sig
    type 't stream

    val ofArray : 't array -> 't stream
    (* val toSeq : 't stream -> 't seq *)
    val naturals : int stream

    val map    : ('t -> 'r) -> 't stream -> 'r stream
    val filter : ('t -> bool) -> 't stream -> 't stream
    val takeWhile : ('t -> bool) -> 't stream -> 't stream
    val skipWhile : ('t -> bool) -> 't stream -> 't stream
    val skip : int -> 't stream -> 't stream
    val take : int -> 't stream -> 't stream
    val flatMap: ('t -> 'r stream) -> 't stream -> 'r stream
    val fold   : ('a -> 't -> 'a) -> 'a -> 't stream -> 'a
    val length : 't stream -> int
    val sum :  LargeInt.int stream -> LargeInt.int
    val iter : ('t -> bool) -> 't stream -> bool
end

structure Stream : STREAM = struct 
  (* Streams encode the loop iteration with a looping function that should 
     -return true  if all elements are processed. 
     -return true  if the iteration describes an early termination operator 
                      such as a count, like limit and the count is exhausted.
     -return false if the function passed returns false  
  *) 
  datatype 't stream = Stream of ('t -> bool) -> bool
		    
  (* Creation of stream from an array *)
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
		 );
		 !cont
	      end
      in
	 Stream gen
      end
	 
  (* Iterate through all natural numbers *)
  val naturals = 
      let val gen = fn iterf => recur (0, fn (i, loop) =>
					     iterf i
					     andalso loop (i + 1))
      in Stream gen
      end

  (* Intermediate operators *)
  fun map f (Stream streamf) = 
      Stream (fn iterf => streamf (fn value => iterf (f value)))

  fun filter pred (Stream streamf) = 
      Stream (fn iterf => 
		 streamf(fn value => if pred(value) 
				     then iterf(value) 
				     else true))
	     
  fun flatMap f (Stream outerf) = 
      Stream (fn iterf => 
	outerf(fn value => let val (Stream innerf) = f(value)
			   in
			      innerf(iterf)
			   end
	     ))
	  
  (* Terminal operators *)
  fun fold f a (Stream streamf) = 
      let val x = ref a
      in
	  streamf (fn value => (x := f (!x) value; true));
	  !x
      end
	  
  fun iter f (Stream streamf) = streamf f

  fun length s = fold (fn a => fn _ => a + 1) 0 s

  fun sum s = fold (fn a => fn ss => a + ss) (LargeInt.fromInt 0) s

  (* Early termination operators *)
  fun skip n (Stream streamf) = 
      let val iter = fn iterf => let val count = ref 0 
				     fun check a = let val count' = !count
						   in if count' < n
						      then (count := count' + 1; true)
						      else iterf(a)
						   end
				 in streamf(check)
				 end
      in Stream iter
      end
	 
  fun skipWhile isJunk (Stream streamf) = 
      let val iter = fn iterf => let val pastJunk = ref false
				     fun check a = if !pastJunk
						     then iterf(a)
						   else if isJunk(a)
						     then true
						   else (pastJunk := true; iterf(a))
				 in streamf(check)
				 end
      in Stream iter
      end
	 
  fun take n (Stream streamf) = 
      let val iter = fn iterf => 
			let val count = ref 0 
			    fun check a = let val count' = 1 + !count
					      val () = count := count'
					  in count' <= n 
					     andalso iterf(a)
					  end
			in streamf(check)
			   orelse not (!count <= n)
			end
      in Stream iter
      end

  fun takeWhile pred (Stream streamf) = 
      let val iter = fn iterf =>
			let val early = ref false
			    fun check a = if pred(a)
					  then iterf(a)
					  else (early:=true; false)
			in streamf(check)
			   orelse !early
			end
      in Stream iter
      end
end
