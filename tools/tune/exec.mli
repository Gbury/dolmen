
type cmd = string
type env = string array

module type S = sig

  type t
  type res
  type stats

  val run : ?env:env -> cmd -> (t, string) Result.t
  (** Run a command *)

  val res : t -> res
  (** The underlying result. *)

  val stats : t -> stats
  (** The stats of the result. *)

  val print : Format.formatter -> t -> unit
  (** Print function *)

end

module Core : sig

  type res = {
    errcode : int;
    stdout : string;
    stderr : string;
  }

  type stats = unit

  include S with type res := res and type stats := stats

end

module Time(Base : S) : sig

  type res = Base.t

  type stats = {
    (* time stats *)
    real_time : float;
    user_time : float;
    kernel_time : float;
    (* Memory stats *)
    peak_mem : int;
  }

  include S with type res := res and type stats := stats

end

module Full : sig

  type res = Core.t

  type stats = {
    (* time stats *)
    real_time : float;
    user_time : float;
    kernel_time : float;
    (* Memory stats *)
    peak_mem : int;
  }

  include S with type res := res and type stats := stats

end

