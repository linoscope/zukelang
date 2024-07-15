open Zukelang

module type Hash = sig
  val size : int (** in bits *)
  val hash : bytes -> bytes
end

module SHA2_256 = struct
  let hash = Hacl_star.Hacl.SHA2_256.hash
  let size = Bytes.length (hash (Bytes.empty))
end

(* Simple NIZK, proving that the prover knows a private key $a$ *)
module Simple(Hash : Hash)(Fr : Curve.F)(G : Curve.G with type fr = Fr.t) = struct

  (* Naming is not good. G.fr is only meaningful when #G = #Fr *)
  let prove rng (a : Fr.t) =
    let n = Fr.order in
    (* public key A = aG *)
    let _A = G.of_Fr a in
    (* random v = [1,n-1] *)
    let v = Fr.of_z Z.(random_int ~rng (n - one) + one) in
    (* V = vG *)
    let _V = G.of_Fr v in
    (* c = H( G || V || A ) *)
    let c =
      Fr.of_z @@
      Z.of_bits @@ Bytes.unsafe_to_string
      @@ Hash.hash @@ Bytes.unsafe_of_string
      @@ Format.asprintf "%a %a %a" G.pp G.one G.pp _V G.pp _A
    in
    (* r = v - ac *)
    let r = Fr.(v - a * c) in
    (_A, _V, c, r)

  let verify (_A, _V, c, r) =
    (* XXXX A is on the curve. No such funciton for now *)
    (* Check c is correct *)
    let c' =
      Fr.of_z @@
      Z.of_bits @@ Bytes.unsafe_to_string
      @@ Hash.hash @@ Bytes.unsafe_of_string
      @@ Format.asprintf "%a %a %a" G.pp G.one G.pp _V G.pp _A
    in
    if not Fr.(c = c') then false
    else
      let rG = G.of_Fr r in
      let cA = G.(_A * c) in
      (* V = rG + cA *)
      G.(_V = rG + cA)

end

let test () =
  let module Fr = Curve.Bls12_381.Fr in
  let module G = struct
    include Curve.Bls12_381.G1
    type fr = Curve.Bls12_381.Fr.t
  end in
  let module NIZK = Simple(SHA2_256)(Fr)(G) in
  let rng = Random.State.make_self_init () in
  (* Private key a *)
  let a = Fr.of_z Z.(random_int ~rng (Fr.order - one) + one) in
  let proof = NIZK.prove rng a in
  assert (NIZK.verify proof);
  prerr_endline "Nizk.Simple successful"
