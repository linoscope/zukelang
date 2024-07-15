open Misclib

(** Cyclic additive group of a prime order *)
module type G = sig
  type t (** Element of G *)

  type fr (** Scalar of field Fr *)

  val order : Z.t

  val zero : t (** O *)
  val one : t (** Generator G *)
  val ( ~- ) : t -> t
  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> fr -> t (** Scalar multiplication *)
  val ( = ) : t -> t -> bool
  val sum : t list -> t
  val of_Fr : fr -> t (** xG *)
  val of_q : Q.t -> t
  val pp : t printer

  val sum_map : 'a Var.Map.t -> (Var.t -> 'a -> t) -> t
  (** $\mathrm{sum\_map}(m, f) = \Sigma_{k\in Dom(m)} f(k, m(k))$ *)

  val dot : t Var.Map.t -> fr Var.Map.t -> t
  (** $\mathrm{dot}(m,c) = \Sigma_{k\in Dom(m)} m(k) \cdot c(k)$ *)

  val powers : int -> fr -> t list
  (** $\mathrm{powers}(d, s) = \{ g^s^i \}_{i\in[d]}$ *)

  val apply_powers : fr Polynomial.t -> t list -> t
  (** $\mathrm{apply\_powers}(\Sigma_i c_i x^i, [v^i]) = \Sigma_i c_i v^i$ *)

  include JSON.Conv.S with type t := t
end

(** Finaite field *)
module type F = sig
  include Field.COMPARABLE
  include G with type t := t and type fr := t
  module Poly : Polynomial.S with type f = t

  val order : Z.t

  val ( ** ) : t -> Z.t -> t

  val gen : t Gen.t

end

module type S = sig
  module Fr : F
  module G1 : G with type fr := Fr.t
  module G2 : G with type fr := Fr.t
  module GT : G with type fr := Fr.t
  module Pairing : sig
    val pairing : G1.t -> G2.t -> GT.t
  end
end

module Bls12_381 : S
  with type Fr.t = Bls12_381.Fr.t
   and type G1.t = Bls12_381.G1.t
   and type G2.t = Bls12_381.G2.t
   and type GT.t = Bls12_381.GT.t

module Root_of_unity(F : F) : sig

  (* [f_of_uint base x] computes the [F.t] value for $x$ of [base] bit uint *)
  val f_of_uint : int -> int -> F.t option

end
