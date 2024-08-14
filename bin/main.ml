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

type statement =
| Let of symbol * set
| Case of {
    state: symbol;
    read: symbol;
    write: symbol;
    move: symbol;
    next: symbol;
  }
| Run of {
    trace: bool;
    entry_state: symbol;
    tape: tape;
  }
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

let parse_statements lex =
  let f lex =
    lex
    |> Seq.uncons
    |> Option.map (function
      | ("let", _lex) -> parse_let lex
      | ("case", _lex) -> parse_case lex
      | (("run" | "trace"), _lex) -> parse_run lex
      | (other, _lex) -> not_expected "keyword let, case, run or trace" (Some other)
    )
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

let () =
  let source =
    In_channel.with_open_bin "bin/input.tula" @@
      fun f -> In_channel.input_all f
  in
  let ast = parse source in
  print_endline source;
  Format.printf "%a@." pp_ast ast;
  ignore ast

