module Make(F : Field.COMPARABLE) : sig

  type t =
    { v : Polynomial.Make(F).t Var.Map.t;
      w : Polynomial.Make(F).t Var.Map.t;
      y : Polynomial.Make(F).t Var.Map.t;
      target : Polynomial.Make(F).t }

  val build : Circuit.Make(F).Gate.Set.t -> t * (int * Circuit.Make(F).Gate.t) list

  val decompile : t -> (int * Circuit.Make(F).Gate.t) list -> Circuit.Make(F).Gate.Set.t

  val eval : F.t Var.Map.t -> t -> Polynomial.Make(F).t * Polynomial.Make(F).t
  (** compute $p$ and $h$ *)
end