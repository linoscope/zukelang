open Utils

type binop = Add | Mul

module Make(F : Field.S) : sig
  module Term : sig
    type term = Var of Var.t | Num of F.t

    type t = term

    val pp : t printer
  end

  module Expr : sig

    type expr = Term of Term.t | BinApp of binop * expr * expr

    type t = expr

    val pp : t printer

    val var : string -> t
    val num : F.t -> t
    val mul : t -> t -> t
    val add : t -> t -> t

    module Infix : sig
      val ( + ) : t -> t -> t
      val ( * ) : t -> t -> t
      val ( !! ) : F.t -> t
      val ( !!! ) : int -> t
      val ( ?? ) : string -> t
    end

    val eval : (Var.t * F.t) list -> t -> F.t
  end

  module Flatten : sig

    type flatten = Var.t * binop * Term.t * Term.t

    type t = flatten

    val pp : t printer

    val flatten : Var.t * Expr.t -> t list

    val vars : t list -> Var.Set.t

    val eval : (Var.t * F.t) list -> t list -> (Var.t * F.t) list
  end
end