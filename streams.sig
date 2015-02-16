(* Aggelos Biboudis
 * 
 * Stream library *)

signature STREAM = sig
    type 't stream
    val ofArray : 't array -> 't stream
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
