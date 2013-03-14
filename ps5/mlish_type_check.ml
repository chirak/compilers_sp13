open Mlish_ast

exception TypeError
exception EnvLookup

module Var_set = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = tipe
  end );;

let empty = Var_set.empty;;

let type_error(s:string) =
  (print_string s; raise TypeError)
;;

type env = (var*tipe_scheme) list;;

let rec lookup (v:string) (env:env) =
  match env with
      [] -> raise EnvLookup
    | hd::tl ->
        let v',e = hd in
          if v = v' then e else lookup v tl
;;

let type_check_exp (e:exp) : tipe =

  let rec andmap f l =
    match l with
        [] -> true
      | hd::tl -> (f hd) && (andmap f tl)
  in

  let guess() = Guess_t (ref None) in

  let rec unify (t1:tipe) (t2:tipe):bool =
    if (t1 = t2) then true else 
      match t1,t2 with 
          Guess_t({contents = (Some t1')}), _ -> unify t1' t2 
        | Guess_t (({contents = None}) as r), _ ->
            (r.contents <- Some(t2); true) 
        | _, Guess_t(_) -> unify t2 t1 
        | Fn_t(t1a,t1b), Fn_t(t2a,t2b) | Pair_t(t1a, t1b), Pair_t(t2a, t2b) -> 
            unify t1a t2a && unify t1b t2b
        | List_t(a), List_t(b) -> unify a b
        | Tvar_t(a), Tvar_t(b) -> a = b
        | _ -> 
          print_string "Unify Failed";
          raise TypeError
  in

  let var_counter = ref 0 in
  let new_int() = (var_counter := (!var_counter) + 1; !var_counter) in
  let new_var() = "t" ^ (string_of_int (new_int())) in

  let rec lookup' v = function
    | (a, g)::t when a = v -> g
    | _::t -> lookup' v t
    | [] -> raise TypeError
  in

  let rec substitute vs_and_ts = function
    | Tvar_t(x) -> lookup' x vs_and_ts
    | List_t(x) -> List_t(substitute vs_and_ts x)
    | Pair_t(a, b) ->
        Pair_t(substitute vs_and_ts a, substitute vs_and_ts b)
    | Fn_t(a, b) -> 
        Fn_t(substitute vs_and_ts a, substitute vs_and_ts b)
    | t -> t
  in

  let generalize ((e:env), (t:tipe)) : tipe_scheme =

    let rec guesses_of_tipe s = function
      | Guess_t(x) as t -> Var_set.add t s
      | List_t(x) -> guesses_of_tipe s x
      | Pair_t(a, b) | Fn_t(a, b) -> 
          guesses_of_tipe (guesses_of_tipe s a) b
      | _ -> s
    in
    let guesses_of (Forall (_, t)) = guesses_of_tipe empty t in

    let type_guesses = guesses_of_tipe empty t in
    let env_guesses = List.fold_left Var_set.union empty
                        (List.map (fun (x,s) -> guesses_of s) e) 
    in
    let frame_vars = Var_set.diff type_guesses env_guesses in
    let gs_vs = 
      List.map (fun g -> (new_var(), g)) (Var_set.elements frame_vars)
    in
    let t' = substitute gs_vs t in
      Forall(List.map (function (v, _) -> v) gs_vs, t')
  in

  let instantiate (Forall(vs,t)) : tipe =
    let vs_and_ts = List.map (fun a -> (a, guess())) vs in
      substitute vs_and_ts t
  in

  let rec type_check_prim (p:prim) (exprs:exp list) (env:env) : tipe =
    match p with
        Int(i)  ->
          (match exprs with
               [] -> Int_t
             | _ -> raise TypeError)
      | Bool(b) ->
        (match exprs with
             [] -> Bool_t
           | _ -> raise TypeError)
      | Unit ->
        (match exprs with
             [] -> Unit_t
           | _ -> raise TypeError)
      | Plus | Minus | Times | Div ->
          (match exprs with
               [i1;i2] ->
                 let _ = print_string "HERE\n" in
                 (match type_check' i1 env, type_check' i2 env with
                      Int_t, Int_t -> Int_t
                    | _ -> raise TypeError)
             | _ -> raise TypeError)
      | Lt ->
          (match exprs with
               [i1;i2] ->
                 (match type_check' i1 env, type_check' i2 env with
                      Int_t, Int_t -> Bool_t
                    | _ -> raise TypeError)
             | _ -> raise TypeError)
      | Eq ->
          (match exprs with
               [i1;i2] ->
                 (match type_check' i1 env, type_check' i2 env with
                      t1, t2 when t1 = t2 -> t1
                    | _ -> raise TypeError)
             | _ -> raise TypeError)
      | Pair ->
          (match exprs with
               [x1;x2] -> Pair_t(type_check' x1 env,type_check' x2 env)
             | _ -> raise TypeError)
      | Fst ->
          let rec unnest_guess = function
            | Guess_t(t) -> 
              (match !t with
                | Some t -> unnest_guess t
                | None -> 
                    let t_fst = guess() in
                    t := Some(Pair_t(t_fst, guess()));
                    t_fst)
            | Pair_t(t, _) -> t
          in
          (match exprs with
               [x] -> unnest_guess (type_check' x env)
             | _ -> raise TypeError)
      | Snd ->
          let rec unnest_guess = function
            | Guess_t(t) -> 
              (match !t with
                | Some t -> unnest_guess t
                | None -> 
                    let t_snd = guess() in
                    t := Some(Pair_t(guess(), t_snd));
                    t_snd)
            | Pair_t(_, t) -> t
          in
          (match exprs with
               [x] -> unnest_guess (type_check' x env)
             | _ -> raise TypeError)
      | Nil -> 
          (match exprs with
                [] -> guess()
              | _ -> raise TypeError)
      | Cons ->
          (match exprs with
               [hd;tl] ->
                 let hd_t = type_check' hd env in
                   (match type_check' tl env with
                        List_t(tl_t) as t when tl_t = hd_t -> t
                      | _ -> raise TypeError)
             | _ -> raise TypeError)
      | IsNil ->
          let rec unnest_guess = function
            | Guess_t(t) -> 
              (match !t with
                | Some t -> unnest_guess t
                | None -> 
                    t := Some(List_t(guess()));
                    Bool_t)
            | List_t(_) -> Bool_t
          in
          (match exprs with
               [l] -> unnest_guess (type_check' l env)
             | _ -> raise TypeError)
      | Hd ->
          let rec unnest_guess = function
            | Guess_t(t) -> 
              (match !t with
                | Some t -> unnest_guess t
                | None -> 
                    let t' = guess() in
                    t := Some(List_t(t'));
                    t')
            | List_t(t) -> t
          in
          (match exprs with
               [l] ->
                 unnest_guess (type_check' l env)
             | _ -> raise TypeError)
      | Tl ->
          let rec unnest_guess = function
            | Guess_t(t) -> 
              (match !t with
                | Some t -> unnest_guess t
                | None -> 
                    let t' = List_t(guess()) in
                    t := Some(t');
                    t')
            | List_t(t) as lit -> lit
          in
          (match exprs with
               [l] ->
                 unnest_guess (type_check' l env)
             | _ -> raise TypeError)
      | _ -> raise TypeError

  and type_check' ((e,_):exp) (env:env) : tipe =
    match e with
        Var(v) ->
          print_string "MATCHING VAR\n";
          (* let env_str = List.map (fun (n,t) -> n^" "^(ts2string t)) env in *)
          (* let l_str = String.concat ", " env_str in *)
          (* let _ = Printf.printf "%s\n" l_str in *)
          instantiate(lookup v env)
      | PrimApp(p, exps) ->
          print_string "MATCHING PRIMAPP\n";
          type_check_prim p exps env
      | Fn(v, exp) ->
          print_string "MATCHING FUNCTION\n";
          let t = guess() in
          let body_type = type_check' exp ((v, Forall([], t))::env) in
            Fn_t(t, body_type)
      | App(e1, e2) -> 
          print_string "MATCHING APP\n";
          print_string "checking e1";
          let t1 = type_check' e1 env in
          print_string "checking e2";
          let t2 = type_check' e2 env in
          let t = guess() in
          let _ = Printf.printf "Fun Type: %s\n" (tipe2string t1) in
          let _ = Printf.printf "Arg Type: %s\n" (tipe2string t2) in
            if unify t1 (Fn_t(t2, t)) then
              let _ = Printf.printf "Unified Type: %s\n" (tipe2string t) in
                t 
            else raise TypeError
      | If(test, e1, e2) ->
          print_string "matching IF\n";
          (match type_check' test env with
               Bool_t ->
                 (match type_check' e1 env, type_check' e2 env with
                      a,b when a = b -> a
                    | _ -> raise TypeError)
             | _ -> raise TypeError)
      | Let(v, e1, e2) ->
          print_string "matching LET\n";
          let s = generalize(env, type_check' e1 env) in
          let _ = Printf.printf "%s\n" (ts2string s) in
            type_check' e2 ((v,s)::env)
  in
    type_check' e []
;;
