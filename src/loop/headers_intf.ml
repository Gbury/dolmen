
module type S = sig

  type state
  (** global state threaded through all the pipeline *)

  val check : state -> state
  (** Check a state for the required headers (once a whole pipeline
      has been completed *)

  val inspect :
    state -> Dolmen.Std.Statement.t -> state * Dolmen.Std.Statement.t
  (** Check the headers *)

end
