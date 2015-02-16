(* Aggelos Biboudis *)

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
	  
  fun iter f (Stream streamf) = 
      streamf f

  fun length s = fold (fn a => fn _ => a + 1) 0 s

  fun sum s = fold (fn a => fn ss => a + ss) (LargeInt.fromInt 0) s

  fun filter pred (Stream streamf) = 
      Stream (fn iterf => 
		 streamf(fn value => if pred(value) 
				     then iterf(value) 
				     else true))

  fun takeWhile pred (Stream streamf) = 
      Stream (fn iterf => 
		 streamf(fn value => if pred(value) 
				     then iterf(value) 
				     else false))

  fun skip n (Stream streamf) = 
      let val iter = fn iterf => 
			let val count = ref 0 in
			    streamf(fn value => 
				       let val _ = count := !count+1
				       in
					   if !count > n
					   then iterf(value) 
					   else true
				       end)
			end
      in
	  Stream iter
      end

  fun skipWhile pred (Stream streamf) = 
      Stream (fn iterf => 
		 streamf(fn value => 
			    let val shortcut = ref true
			    in
				if !shortcut andalso pred(value)
				then true
				else 
				    let val _ = shortcut := true
				    in
					iterf(value)
				    end
			    end))

  fun take n (Stream streamf) = 
      let val iter = fn iterf => 
			let val count = ref 0 in
			  streamf(fn value => 
				     let val _ = count := !count+1
				     in
					 if !count <= n
					 then iterf(value) 
					 else false
				     end)
			end
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
