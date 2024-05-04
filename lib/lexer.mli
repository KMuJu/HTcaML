type t

(** Creates lexer from input string *)
val init : string -> t

(** Gets the next lexer and token option *)
val next_token : t -> t * Token.t option

(** Pretty printing *)
val pp : Format.formatter -> t -> unit
(** Pretty printing *)
val show : t -> string
