module Lexer = struct
  type token = string
    [@@deriving show]

  type t = token Seq.t

  let is_whitespace c =
    Char.equal ' ' c || Char.equal '\n' c

  let is_token c =
    not (is_whitespace c)

  let next_token chars =
    let prefix =
      chars
      |> Seq.take_while (is_token)
      |> String.of_seq
    in
    let chars = chars |> Seq.drop (String.length prefix) in
    let chars = chars |> Seq.drop_while (is_whitespace) in
    (prefix, chars)

  let make src : t =
    let chars = String.to_seq src in
    let chars = chars |> Seq.drop_while (is_whitespace) in
    let next_token chars =
      match next_token chars with
      | ("", _) -> None
      | other -> Some other
    in
    Seq.unfold next_token chars

  (* tests *)
  let () =
    assert (make "" |> Seq.uncons |> Option.is_none);
    assert (make " " |> Seq.uncons |> Option.is_none);
    assert (make " \n" |> Seq.uncons |> Option.is_none);
    assert (make " ab bc \n" |> List.of_seq = ["ab"; "bc"]);
    ()
end

type symbol = Symbol of Lexer.token
  [@@deriving show]

let not_expected expected got =
  let got =
    match got with
    | None -> "nothing"
    | Some g -> g
  in
  failwith (Format.sprintf "Expected %s, got %s" expected got)

let parse_symbol lex =
  match lex |> Seq.uncons with
  | Some (token, lex) -> (Symbol token, lex)
  | None -> not_expected "symbol" None

(* tests *)
let () =
  begin
    let l = Lexer.make " ab bc \n" in
    let ((Symbol x), l) = parse_symbol l in
    assert (x = "ab");
    let ((Symbol x), l) = parse_symbol l in
    assert (x = "bc");
    let x = Seq.uncons l in
    assert (x = None);
    ()
  end

type set = symbol list
  [@@deriving show]

type tape = symbol list
  [@@deriving show]

type run = {
  trace: bool;
  entry_state: symbol;
  tape: tape;
}
  [@@deriving show]

type statement =
| Let of symbol * set
| For of {
  variable: symbol; (* TODO: collection here *)
  set: symbol;
  body: statement; (* TODO: collection here *)
}
| Case of {
    state: symbol;
    read: symbol;
    write: symbol;
    move: symbol;
    next: symbol;
  }
| Run of run
  [@@deriving show]

let expect token l =
  match l |> Seq.uncons with
  | Some (t, l) when t = token -> (t, l)
  | Some (t, _l) -> not_expected token (Some t)
  | None -> not_expected token None

let parse_symbols_in_braces lex =
  let _obr, lex = expect "{" lex in
  let rec go lex rev_syms =
    match lex |> Seq.uncons with
    | Some ("}", lex) -> List.rev rev_syms, lex
    | Some (_other, _lex) ->
        let sym, lex = parse_symbol lex in
        go lex (sym :: rev_syms)
    | None -> not_expected "symbol or }" None
  in
  go lex []

let parse_set = parse_symbols_in_braces
let parse_tape = parse_symbols_in_braces

let parse_let lex =
  let _let, lex = expect "let" lex in
  let name, lex = parse_symbol lex in
  let set, lex = parse_set lex in
  Let (name, set), lex

(* parse_statement here is open recursion..
   The actual parse_statement should pass itself
   as argumebt when calling this function.
   I used open recursion here just to avoid
   having to declare these functions together
   in the file with `and` as separator. *)
let parse_for parse_statement lex =
  let _for, lex = expect "for" lex in
  let variable, lex = parse_symbol lex in
  let _in, lex = expect "in" lex in
  let set, lex = parse_symbol lex in
  let body, lex = parse_statement lex in
  For { variable; set; body }, lex

let parse_case lex =
  let _case, lex = expect "case" lex in
  let state, lex = parse_symbol lex in
  let read, lex = parse_symbol lex in
  let write, lex = parse_symbol lex in
  let move, lex = parse_symbol lex in
  let next, lex = parse_symbol lex in
  Case { state; read; write; move; next; }, lex

let parse_run lex =
  let trace, lex =
    match lex |> Seq.uncons with
    | Some ("run", lex) -> false, lex
    | Some ("trace", lex) -> true, lex
    | Some (other, _lex) -> not_expected "run or trace" (Some other)
    | None -> not_expected "run or trace" None
  in
  let entry_state, lex = parse_symbol lex in
  let tape, lex = parse_tape lex in
  Run { trace; entry_state; tape; }, lex

let rec parse_statement lex =
  match lex |> Seq.uncons with
  | Some ("let", _lex) -> parse_let lex
  | Some ("for", _lex) -> parse_for parse_statement lex
  | Some ("case", _lex) -> parse_case lex
  | Some (("run" | "trace"), _lex) -> parse_run lex
  | Some (other, _lex) -> not_expected "keyword let, case, run or trace" (Some other)
  | None -> not_expected "keyword let, case, run or trace" None

let parse_statements lex =
  let f lex =
    lex
    |> Seq.uncons
    |> Option.map (fun _ -> parse_statement lex)
  in
  Seq.unfold f lex

type ast = Ast of statement list
  [@@deriving show]
let ast xs = Ast xs

let parse src =
  let lex = Lexer.make src in
  parse_statements lex
  |> List.of_seq
  |> ast

module Tape (*: sig
  type 'a t
  type move = Left | Right
  val from_list: 'a list -> 'a t
  val as_list: 'a t -> 'a list
  val write: 'a -> 'a t -> 'a t
  val read: 'a t -> 'a
end*) = struct (* a zipper of a tape *)
  type 'a t = {
    rev_left: 'a list;
    current: 'a;
    right: 'a list;
  }

  type move = Left | Right

  let from_list = function
  | x :: xs -> { rev_left=[]; current=x; right=xs }
  | [] -> failwith "expected non-empty tape"

  let to_list { rev_left; current; right } =
    List.append (List.rev rev_left) (current::right)

  let move s t =
    match s, t with
    | Left, { rev_left=[]; current=_; right=_ } -> None
    | Left, { rev_left=l::ls; current=c; right=rs } -> Some { rev_left=ls; current=l; right=c::rs }
    | Right, { rev_left=_; current=_; right=[] } -> None
    | Right, { rev_left=ls; current=c; right=r::rs } -> Some { rev_left=c::ls; current=r; right=rs }

  let write x { rev_left; current=_; right } =
    { rev_left; current=x; right }

  let read { rev_left=_; current; right=_ } =
    current

  (* tests *)
  let () =
    let t = from_list ["a"; "bc"; "de"; "fgh"] in
    assert (t.rev_left = []);
    assert (t.current = "a");
    assert (t.right = ["bc"; "de"; "fgh"]);
    assert (read t = "a");
    assert (to_list t = ["a"; "bc"; "de"; "fgh"]);
    assert (move Left t = None);
    assert (move Right t |> Fun.flip Option.bind (move Left) = Some t);
    let t' = t in
    let t = move Right t |> Option.get in
    assert (t.rev_left = ["a"]);
    assert (t.current = "bc");
    assert (t.right = ["de"; "fgh"]);
    assert (read t = "bc");
    assert (to_list t = ["a"; "bc"; "de"; "fgh"]);
    assert (move Left t = Some t');
    assert (move Left t |> Fun.flip Option.bind (move Right) = Some t);
    assert (move Right t |> Fun.flip Option.bind (move Left) = Some t);
    (* let t' = t in *)
    let t = write "x0" t in
    assert (t.rev_left = ["a"]);
    assert (t.current = "x0");
    assert (t.right = ["de"; "fgh"]);
    assert (read t = "x0");
    assert (to_list t = ["a"; "x0"; "de"; "fgh"]);
    (* assert (move Left t = Some t'); *)
    assert (move Left t |> Fun.flip Option.bind (move Right) = Some t);
    assert (move Right t |> Fun.flip Option.bind (move Left) = Some t);
    let t' = t in
    let t = move Right t |> Option.get in
    assert (t.rev_left = ["x0"; "a"]);
    assert (t.current = "de");
    assert (t.right = ["fgh"]);
    assert (read t = "de");
    assert (to_list t = ["a"; "x0"; "de"; "fgh"]);
    assert (move Left t = Some t');
    assert (move Left t |> Fun.flip Option.bind (move Right) = Some t);
    assert (move Right t |> Fun.flip Option.bind (move Left) = Some t);
    let t' = t in
    let t = move Right t |> Option.get in
    assert (t.rev_left = ["de"; "x0"; "a"]);
    assert (t.current = "fgh");
    assert (t.right = []);
    assert (read t = "fgh");
    assert (to_list t = ["a"; "x0"; "de"; "fgh"]);
    assert (move Left t = Some t');
    assert (move Left t |> Fun.flip Option.bind (move Right) = Some t);
    assert (move Right t = None);
    (* let t' = t in *)
    let t = write "x1" t in
    assert (t.rev_left = ["de"; "x0"; "a"]);
    assert (t.current = "x1");
    assert (t.right = []);
    assert (read t = "x1");
    assert (to_list t = ["a"; "x0"; "de"; "x1"]);
    (* assert (move Left t = Some t'); *)
    assert (move Left t |> Fun.flip Option.bind (move Right) = Some t);
    (*let t' = t in *)
    assert (move Right t = None);
    ()
end

(* TODO: consider `trace` *)
let run_once ast { trace=_; entry_state; tape }: unit =
  (* TODO: make a function (s,r)->(w,m,s) of these instead of expanding them *)
  let all_cases =
    ast
    |> List.filter_map (function
      | For _ | Let _ -> None (* TODO: expand these *)
      | Case { state; read; write; move; next; } -> Some ((state, read), (write, move, next))
      | Run _ -> None)
    |> List.to_seq
    |> Hashtbl.of_seq
  in
  let tape = Tape.from_list tape in
  let transition (state, (tape: symbol Tape.t)) =
    let (let*) = Option.bind in
    let read = Tape.read tape in
    let* (write, move, next) = Hashtbl.find_opt all_cases (state, read) in
    let move = match move with
      | Symbol "->" -> Tape.Right
      | Symbol "<-" -> Tape.Left
      | Symbol _ -> failwith "unexpected move. it must be either -> or <-"
    in
    let* tape = tape |> Tape.write write |> Tape.move move in
    let twice x = x, x in
    Option.some @@ twice (next, tape)
  in
  let print_state (state, tape) =
    let pp_symbol fmt (Symbol s) = Format.fprintf fmt "%s" s in
    let pp_symbol_list = Format.pp_print_list ~pp_sep:(fun fmt () -> Format.fprintf fmt " ") pp_symbol in
    let print_with_marker_under_element prefix element suffix =
      let buff = Buffer.create 0 in
      let fmt = Format.formatter_of_buffer buff in
      Format.fprintf fmt "%t@?" prefix; (* @? important for buf len *)
      let len0 = buff |> Buffer.length in
      Format.fprintf fmt "%t@?" element; (* @? important for buf len *)
      let len1 = buff |> Buffer.length in
      Format.fprintf fmt "%t@\n" suffix;
      Format.fprintf fmt "%s^%s@." (* should this end with `@.` or with `@?` ? *)
        (String.make len0 ' ')
        (String.make (len1 - len0 - 1) '~');
      Format.dprintf "%s" (Buffer.contents buff);
    in
    (* Format.printf "%a: %a@." pp_symbol state pp_symbol_list (tape |> Tape.to_list) *)
    let m = (Format.dprintf "%a" pp_symbol tape.Tape.current) in
    let l =
      match tape.rev_left with
      | [] -> (Format.dprintf "%a: " pp_symbol state)
      | ls -> (Format.dprintf "%a: %a "  pp_symbol state pp_symbol_list (ls |> List.rev))
    in
    let r =
      match tape.right with
      | [] -> (Format.dprintf "")
      | rs -> (Format.dprintf " %a" pp_symbol_list rs)
    in
    Format.printf "%t" (print_with_marker_under_element l m r);
    ()
  in
  let states = Seq.unfold transition (entry_state, tape) in
  print_state (entry_state, tape);
  states |> Seq.iter print_state;
  Format.printf "@."

let run (Ast ast) =
  let runs =
    ast |> List.filter_map @@ function
      | For _ | Let _ | Case _ -> None
      | Run r -> Some r
  in
  runs
  |> List.iter (run_once ast)

let () =
  let source =
    In_channel.with_open_bin "bin/input.tula" @@
      fun f -> In_channel.input_all f
  in
  let ast = parse source in
  print_endline source;
  Format.printf "%a@." pp_ast ast;
  run ast

