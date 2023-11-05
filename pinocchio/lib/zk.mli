open Utils

module Make(C : Ecp.CURVE) : sig
  type expr = Lang.Make(C.Fr).Expr.t
  type circuit = Circuit.Make(C.Fr).t
  type qap = QAP.Make(C.Fr).t

  type ekey (** Evaluation key *)

  type vkey (** Verificaiton key *)

  type proof

  val compile : expr -> circuit * qap
  (** Compile an expression to a circuit and a QAP *)

  val keygen : circuit -> qap -> ekey * vkey
  (** Key generation *)

  val solve : circuit -> C.Fr.t Var.Map.t -> C.Fr.t Var.Map.t
  (** Evaluate the circuit with the given input *)

  val output_of_solution : circuit -> C.Fr.t Var.Map.t -> C.Fr.t Var.Map.t
  (** The output of the solution *)

  val prove : qap -> ekey -> C.Fr.t Var.Map.t -> proof
  (** Obtain a proof of the computation *)

  val verify : C.Fr.t Var.Map.t -> vkey -> proof -> bool
  (** Verify the computation proof *)

  val zkkeygen : circuit -> qap -> Var.Set.t -> ekey * vkey
  (** ZK Key generation with input hiding *)

  val zkprove : qap -> ekey -> C.Fr.t Var.Map.t -> proof
  (** Obtain a ZK proof of the computation *)
end

val test : unit -> unit