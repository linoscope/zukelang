open Utils

module Make(F : Field.COMPARABLE) : sig

  type 'a ty =
    | Field : F.t ty
    | Bool : bool ty

  type security = Public | Secret

  (** Type of ZK computation *)
  type 'a t =
    | Field : F.t -> F.t t
    | Bool : bool -> bool t
    | Add : F.t t * F.t t -> F.t t
    | Sub : F.t t * F.t t -> F.t t
    | Mul : F.t t * F.t t -> F.t t
    | Div : F.t t * F.t t -> F.t t
    | Input : Var.t * security * 'a ty -> 'a t
    | Not : bool t -> bool t
    | And : bool t * bool t -> bool t
    | Or : bool t * bool t -> bool t
    | If : bool t * 'a t * 'a t -> 'a t
    | Eq : 'a t * 'a t -> bool t
    | To_field : _ t -> F.t t (* cast *)
    | Cast : _ t -> 'a t
    | Let : Var.t * 'a t * 'b t -> 'b t
    | Var : Var.t -> 'a t
    | Neg : F.t t -> F.t t

  val pp : _ t printer

  val var : unit -> Var.t

  module S : sig

    val ty_field : F.t ty
    val ty_bool : bool ty

    type nonrec security = security = Public | Secret

    val public : security
    val secret : security

    val (+) : F.t t -> F.t t -> F.t t
    val (-) : F.t t -> F.t t -> F.t t
    val ( * ) : F.t t -> F.t t -> F.t t
    val (/) : F.t t -> F.t t -> F.t t
    val (~-) : F.t t -> F.t t

    val field : F.t -> F.t t
    val (!) : int -> F.t t
    val bool : bool -> bool t

    val not : bool t -> bool t
    val (&&) : bool t -> bool t -> bool t
    val (||) : bool t -> bool t -> bool t
    val if_ : bool t -> 'a t -> 'a t -> 'a t

    val input : security -> 'a ty -> 'a t

    val to_field : 'a t -> F.t t

    val var : Var.t -> _ t

    val let_ : Var.t -> _ t -> 'b t -> 'b t

    val (==) : 'a t -> 'a t -> bool t

    val cast : _ t -> 'a t
  end

  type value =
    | Field of F.t
    | Bool of bool

  type env = (Var.t * value) list

  val eval : env -> 'a t -> value
end
