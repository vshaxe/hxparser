module type JsonApi = sig
	type t
	val jarray : t list -> t
	val jobject : (string * t) list -> t
	val jbool : bool -> t
	val jstring : string -> t
	val jint : int -> t
	val jnull : t
end
