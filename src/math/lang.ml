open Misclib

module Make(F : Field.COMPARABLE) = struct

  type security = Public | Secret

  module Type = struct
    type _ t =
      | Field : F.t t
      | Bool : bool t
      | Pair : 'a t * 'b t -> ('a * 'b) t
      | Either : 'a t * 'b t -> ('a, 'b) Either.t t

    let rec equal : type a b . a t -> b t -> (a, b) GADT.eq option = fun a b ->
      let open GADT in
      match a, b with
      | Field, Field -> Some Refl
      | Bool, Bool -> Some Refl
      | Pair (t11, t12), Pair (t21, t22) ->
          (match equal t11 t21 with
           | None -> None
           | Some Refl ->
               match equal t12 t22 with
               | None -> None
               | Some Refl -> Some Refl)
      | Either (t11, t12), Either (t21, t22) ->
          (match equal t11 t21 with
           | None -> None
           | Some Refl ->
               match equal t12 t22 with
               | None -> None
               | Some Refl -> Some Refl)
      | _ -> None
  end

  module Expr = struct
    (** Type of ZK computation *)
    type 'a t =
      { desc : 'a desc;
        ty : 'a Type.t
      }

    and 'a desc =
      | Field : F.t -> F.t desc
      | Bool : bool -> bool desc
      | Add : F.t t * F.t t -> F.t desc
      | Sub : F.t t * F.t t -> F.t desc
      | Mul : F.t t * F.t t -> F.t desc
      | Div : F.t t * F.t t -> F.t desc
      | Input : Var.t * security -> 'a desc
      | Not : bool t -> bool desc
      | And : bool t * bool t -> bool desc
      | Or : bool t * bool t -> bool desc
      | If : bool t * 'a t * 'a t -> 'a desc
      | Eq : 'a t * 'a t -> bool desc
      | To_field : _ t -> F.t desc (* cast *)
      | Let : Var.t * 'a t * 'b t -> 'b desc
      | Var : Var.t -> 'a desc
      | Neg : F.t t -> F.t desc
      | Pair : 'a t * 'b t -> ('a * 'b) desc
      | Fst : ('a * 'b) t -> 'a desc
      | Snd : ('a * 'b) t -> 'b desc
      | Left : 'a t -> ('a, _) Either.t desc
      | Right : 'b t -> (_, 'b) Either.t desc
      | Case : ('a, 'b) Either.t t * Var.t * 'c t * Var.t * 'c t -> 'c desc

    let ptree e =
      let rec ptree : type a. a t -> Ppxlib_ast.Ast.expression = fun e ->
        let loc = Location.none in
        let open Ppxlib_ast.Ast_helper in
        match e.desc with
        | Field f -> Exp.constant @@ Const.integer (Format.asprintf "%a" F.pp f)
        | Bool true -> [%expr true]
        | Bool false -> [%expr false]
        | Add (t1, t2) -> [%expr [%e ptree t1] + [%e ptree t2]]
        | Sub (t1, t2) -> [%expr [%e ptree t1] - [%e ptree t2]]
        | Mul (t1, t2) -> [%expr [%e ptree t1] * [%e ptree t2]]
        | Div (t1, t2) -> [%expr [%e ptree t1] / [%e ptree t2]]
        | Input (v, Public) ->
            [%expr ([%e Exp.ident { txt= Longident.Lident (Var.to_string v); loc= Location.none }] : public)]
        | Input (v, Secret) ->
            [%expr ([%e Exp.ident { txt= Longident.Lident (Var.to_string v); loc= Location.none }] : secret)]
        | Var v -> Exp.ident { txt= Longident.Lident (Var.to_string v); loc= Location.none }
        | Not b -> [%expr not [%e ptree b]]
        | And (t1, t2) -> [%expr [%e ptree t1] && [%e ptree t2]]
        | Or (t1, t2) -> [%expr [%e ptree t1] || [%e ptree t2]]
        | If (t1, t2, t3) -> [%expr if [%e ptree t1] then [%e ptree t2] else [%e ptree t3]]
        | Eq (t1, t2) -> [%expr [%e ptree t1] == [%e ptree t2]]
        | To_field t -> [%expr to_field [%e ptree t]]
        | Let (v, t1, t2) -> [%expr let [%p Pat.var {txt= Var.to_string v; loc= Location.none}] = [%e ptree t1] in [%e ptree t2]]
        | Neg t -> [%expr ~- [%e ptree t]]
        | Pair (a, b) -> [%expr ([%e ptree a], [%e ptree b])]
        | Fst a -> [%expr fst [%e ptree a]]
        | Snd a -> [%expr snd [%e ptree a]]
        | Left a -> [%expr Left [%e ptree a]]
        | Right a -> [%expr Right [%e ptree a]]
        | Case (ab, va, a, vb, b) ->
            [%expr
              match [%e ptree ab] with
              | Left [%p Pat.var {txt= Var.to_string va; loc= Location.none}] -> [%e ptree a]
              | Right [%p Pat.var {txt= Var.to_string vb; loc= Location.none}] -> [%e ptree b]]
      in
      ptree e

    let pp ppf t = Ppxlib_ast.Pprintast.expression ppf @@ ptree t

    module C = struct
      let ty_field : _ Type.t = Field

      let ty_bool : _ Type.t = Bool

      let ty_pair t1 t2 : _ Type.t = Pair (t1, t2)

      let ty_either t1 t2 : _ Type.t = Either (t1, t2)

      let public = Public
      let secret = Secret

      let mk desc ty = { desc; ty }

      let bool b = mk (Bool b) ty_bool

      let field n = mk (Field n) ty_field

      let (!) n = mk (Field (F.of_int n)) ty_field

      let (+) a b = mk (Add (a, b)) ty_field

      let (-) a b = mk (Sub (a, b)) ty_field

      let (~-) a = mk (Neg a) ty_field

      let ( * ) a b = mk (Mul (a, b)) ty_field

      let (/) a b = mk (Div (a, b)) ty_field

      let not a = mk (Not a) ty_bool

      let (&&) a b = mk (And (a, b)) ty_bool

      let (||) a b = mk (Or (a, b)) ty_bool

      let if_ a b c = mk (If (a, b, c)) b.ty

      let input sec ty =
        let v = Var.make "input" in
        mk (Input (v, sec)) ty

      let to_field : type a. a t -> F.t t = fun t ->
        match t.ty with
        | Field | Bool -> mk (To_field t) ty_field
        | _ -> invalid_arg "to_field"

      let var v ty = mk (Var v) ty

      let let_ a b =
        let v = Var.make "let" in
        let b = b (var v a.ty) in
        mk (Let (v, a, b)) b.ty

      let (==) a b = mk (Eq (a, b)) ty_bool

      let pair a b = mk (Pair (a, b)) @@ ty_pair a.ty b.ty

      let fst a =
        let ty = match a.ty with
          | Pair (ty, _) -> ty
          | _ -> assert false
        in
        mk (Fst a) ty

      let snd a =
        let ty = match a.ty with
          | Pair (_, ty) -> ty
          | _ -> assert false
        in
        mk (Snd a) ty

      let left a bty = mk (Left a) @@ ty_either a.ty bty

      let right aty b = mk (Right b) @@ ty_either aty b.ty

      let case ab fa fb =
        match ab.ty with
        | Either (aty, bty) ->
            let va = Var.make "case" in
            let vb = Var.make "case" in
            let a = fa (var va aty) in
            let b = fb (var vb bty) in
            mk (Case (ab, va, a, vb, b)) a.ty
        | _ -> assert false
    end
  end

  module Value = struct
    type _ t =
      | Field : F.t -> F.t t
      | Bool : bool -> bool t
      | Pair : 'a t * 'b t -> ('a * 'b) t
      | Left : 'a t -> ('a, _) Either.t t
      | Right : 'b t -> (_, 'b) Either.t t

    type packed = Packed : 'a t * 'a Type.t -> packed

    let unpack : type a. a Type.t -> packed -> a t option =
      fun ty (Packed (t, ty')) ->
      match Type.equal ty ty' with
      | Some GADT.Refl -> Some t
      | None -> None
  end

  module Env = struct
    type t = Value.packed Var.Map.t

    let add v ty value map = Var.Map.add v (Value.Packed (value, ty)) map

    let find v ty env =
      match Value.unpack ty (Var.Map.find v env) with
      | Some v -> v
      | None -> invalid_arg "ill typed"
  end

  module Eval = struct
    let eval env e =
      let rec eval : type a . Env.t -> a Expr.t -> a Value.t = fun env e ->
        let open Expr in
        let field = function
          | Value.Field f -> f
          | _ -> assert false
        in
        let bool = function
          | Value.Bool b -> b
          | _ -> assert false
        in
        match e.desc with
        | Input (v, _sec) -> Env.find v e.ty env
        | Field f -> Field f
        | Bool b -> Bool b
        | Add (a, b) ->
            let a = field @@ eval env a in
            let b = field @@ eval env b in
            Field F.(a + b)
        | Sub (a, b) ->
            let a = field @@ eval env a in
            let b = field @@ eval env b in
            Field F.(a - b)
        | Mul (a, b) ->
            let a = field @@ eval env a in
            let b = field @@ eval env b in
            Field F.(a * b)
        | Div (a, b) ->
            let a = field @@ eval env a in
            let b = field @@ eval env b in
            Field F.(a / b)
        | Not a ->
            let a = bool @@ eval env a in
            Bool (not a)
        | And (a, b) ->
            let a = bool @@ eval env a in
            let b = bool @@ eval env b in
            Bool (a && b)
        | Or (a, b) ->
            let a = bool @@ eval env a in
            let b = bool @@ eval env b in
            Bool (a || b)
        | If (a, b, c) ->
            let a = bool @@ eval env a in
            if a then eval env b else eval env c
        | Eq (a, b) ->
            let a = eval env a in
            let b = eval env b in
            Bool (a = b)
        | To_field a ->
            (match eval env a with
             | Field f -> Field f
             | Bool true -> Field F.one
             | Bool false -> Field F.zero
             | Pair _ | Left _ | Right _ -> assert false)
        | Let (v, a, b) ->
            eval (Env.add v a.ty (eval env a) env) b
        | Var v ->
            Env.find v e.ty env
        | Neg a ->
            let f = field @@ eval env a in
            Field F.(~- f)
        | Pair (a, b) ->
            let a = eval env a in
            let b = eval env b in
            Pair (a, b)
        | Fst a ->
            (match eval env a with
             | Pair (a, _) -> a
             | _ -> assert false)
        | Snd a ->
            (match eval env a with
             | Pair (_, b) -> b
             | _ -> assert false)
        | Left a -> Left (eval env a)
        | Right a -> Right (eval env a)
        | Case (ab, va, a, vb, b) ->
            (match ab.ty with
             | Either (aty, bty) ->
                 (match eval env ab with
                  | Left x -> eval (Env.add va aty x env) a
                  | Right x -> eval (Env.add vb bty x env) b
                  | _ -> assert false)
             | _ -> assert false)
      in
      eval env e
  end
end
