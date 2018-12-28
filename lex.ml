open Sedlexing
open Token

let re_lident = [%sedlex.regexp? lowercase, Star (id_continue | '0'..'9'), Opt '#']
let re_uident = [%sedlex.regexp? uppercase, Star (id_continue | '0'..'9'), Opt '#']
let re_symbol = [%sedlex.regexp? math | other_math | '!' | '$' | '%' | '&' | '|' | '*' | '+' | '-' | '/' | '<' | '=' | '>' | '^' | '~' | '@' | '\\' | '#' | '.' | ':' | '?']
let re_digit = [%sedlex.regexp? '0'..'9']
let re_int = [%sedlex.regexp? Opt '-', Plus ('0'..'9')]
let re_char = [%sedlex.regexp? '\'', any, '\'']
let re_brk_char = [%sedlex.regexp? '\'', '\\', any, '\'']
let re_float = [%sedlex.regexp? Opt '-', Plus ('0'..'9'), '.', Plus ('0'..'9')]
let re_prim_int = [%sedlex.regexp? re_int, '#']
let re_prim_char = [%sedlex.regexp? re_char, '#']
let re_prim_brk_char = [%sedlex.regexp? re_brk_char, '#']
let re_prim_float = [%sedlex.regexp? re_float, '#']
let re_comment = [%sedlex.regexp? ';', ';', Star any]
let re_white = [%sedlex.regexp? Star (Intersect (white_space, Compl '\n'))]
let re_nonempty_white = [%sedlex.regexp? Plus (Intersect (white_space, Compl '\n'))]
let re_ndq = [%sedlex.regexp? Star (Compl '\"')]
let re_string = [%sedlex.regexp? '\"', re_ndq, '\"']

let lex_bol = ref 0
let lex_layout = ref []
let lex_waiting = ref false
let lex_parsing_pkg_sig = ref false
let lex_when_newline = ref None

let add_line posn lexbuf = { posn with
                             Lexing.pos_lnum = posn.Lexing.pos_lnum + 1;
                             Lexing.pos_bol = lexeme_end lexbuf;
                             Lexing.pos_cnum = 0; }

let upd_posn posn lexbuf = { posn with
                             Lexing.pos_cnum = posn.Lexing.pos_cnum + lexeme_length lexbuf }

let lex_action posn buf f =
    if !lex_waiting
    then match !lex_when_newline with
        | Some l -> if posn.Lexing.pos_lnum > l
                then (lex_waiting := false; lex_when_newline := None; let old = posn.Lexing.pos_cnum in lex_layout := old :: !lex_layout; (upd_posn posn buf, INDENT :: f buf))
                else (lex_waiting := false; lex_when_newline := None; (upd_posn posn buf, f buf))
        | None -> lex_waiting := false; let old = posn.Lexing.pos_cnum in lex_layout := old :: !lex_layout; (upd_posn posn buf, INDENT :: f buf)
    else upd_posn posn buf, f buf

type ord =
    | Lt
    | Eq
    | Gt

let offside ind _ = match !lex_layout with
    | [] -> Gt
    | x :: _ ->
        if ind < x
        then Lt
        else if ind = x
        then Eq
        else Gt

let pop_context _ = match !lex_layout with
    | [] -> ()
    | _ :: xs -> lex_layout := xs

let show_posn p = p.Lexing.pos_fname ^ ":" ^ string_of_int p.Lexing.pos_lnum ^ ":" ^ string_of_int p.Lexing.pos_cnum

let rec token_bol ind posn lexbuf =
    match%sedlex lexbuf with
        | '\n', re_nonempty_white -> let nposn = { posn with
                                          Lexing.pos_lnum = posn.Lexing.pos_lnum + 1;
                                          Lexing.pos_bol = lexeme_start lexbuf + 1;
                                          Lexing.pos_cnum = lexeme_length lexbuf - 1 }
                                in
                                token_bol (lexeme_length lexbuf - 1) nposn lexbuf
        | '\n' ->
            let _ = Sedlexing.mark lexbuf in
            token_newline (add_line posn lexbuf) lexbuf
        | re_comment, '\n' -> token (add_line posn lexbuf) lexbuf
        | re_nonempty_white -> token (upd_posn posn lexbuf) lexbuf
        | _ ->
            let rec handle_offside_rule () =
                match offside ind lexbuf with
                    | Lt ->
                        pop_context ();
                        let p, ts = handle_offside_rule () in
                        p, DEDENT :: ts
                    | Eq ->
                        let p, ts = token posn lexbuf in
                        p, SEPARATOR :: ts
                    | Gt -> token posn lexbuf
            in handle_offside_rule ()
and token_newline posn lexbuf =
    match%sedlex lexbuf with
        | Compl '\n' ->
            let rec emit_dedents ts = match !lex_layout with
                | [] -> ts
                | _ :: xs -> lex_layout := xs; emit_dedents (DEDENT :: ts)
            in
            let p, t =
                let _ = Sedlexing.backtrack lexbuf in
                token posn lexbuf
            in
            p, emit_dedents t
        | any ->
            let _ = Sedlexing.backtrack lexbuf in
            token posn lexbuf
        | _ ->
            let _ = Sedlexing.backtrack lexbuf in
            token posn lexbuf
and token posn buf =
    match%sedlex buf with
        | '\n', re_nonempty_white -> let nposn = { posn with
                                          Lexing.pos_lnum = posn.Lexing.pos_lnum + 1;
                                          Lexing.pos_bol = lexeme_start buf + 1;
                                          Lexing.pos_cnum = lexeme_length buf - 1 }
                                     in
                                     token_bol (lexeme_length buf - 1) nposn buf
        | '\n' ->
            let _ = Sedlexing.mark buf in
            token_newline (add_line posn buf) buf
        | re_comment, '\n' -> token (add_line posn buf) buf
        | re_nonempty_white -> token (upd_posn posn buf) buf
        | "let" -> lex_action posn buf (fun _ -> lex_waiting := true; lex_when_newline := Some posn.Lexing.pos_lnum; [LET])
        | "of" -> lex_action posn buf (fun _ -> lex_waiting := true; [OF])
        | "in" -> lex_action posn buf (fun _ -> [IN])
        | "where" -> lex_action posn buf (fun _ ->
            if (not !lex_parsing_pkg_sig)
            then lex_waiting := true
            else lex_parsing_pkg_sig := false;
            [WHERE])
        | '=' -> lex_action posn buf (fun _ -> lex_waiting := true; lex_when_newline := Some posn.Lexing.pos_lnum; [DEF])
        | ';' -> lex_action posn buf (fun _ -> [SEMICOLON])
        | "->" -> lex_action posn buf (fun _ -> [RIGHTARROW])
        | "if" -> lex_action posn buf (fun _ -> [IF])
        | "then" -> lex_action posn buf (fun _ -> [THEN])
        | "else" -> lex_action posn buf (fun _ -> [ELSE])
        | "try" -> lex_action posn buf (fun _ -> [TRY])
        | "catch" -> lex_action posn buf (fun _ -> [CATCH])
        | "while" -> lex_action posn buf (fun _ -> [WHILE])
        | "do" -> lex_action posn buf (fun _ -> [DO])
        | "case" -> lex_action posn buf (fun _ -> [CASE])
        | "class" -> lex_action posn buf (fun _ -> [CLASS])
        | "instance" -> lex_action posn buf (fun _ -> [INSTANCE])
        | "data" -> lex_action posn buf (fun _ -> [DATA])
        | "type" -> lex_action posn buf (fun _ -> [TYPE])
        | "forall" -> lex_action posn buf (fun _ -> [FORALL])
        | "package" -> lex_action posn buf (fun _ -> lex_parsing_pkg_sig := true; [PACKAGE])
        | "infix" -> lex_action posn buf (fun _ -> [INFIX])
        | "infixl" -> lex_action posn buf (fun _ -> [INFIXL])
        | "infixr" -> lex_action posn buf (fun _ -> [INFIXR])
        | "=>" -> lex_action posn buf (fun _ -> [FATRIGHTARROW])
        | "::" -> lex_action posn buf (fun _ -> [ANNOT])
        | "()" -> lex_action posn buf (fun _ -> [UNIT])
        |  '(' -> lex_action posn buf (fun _ -> [LPAREN])
        | ')' -> lex_action posn buf (fun _ -> [RPAREN])
        | '_' -> lex_action posn buf (fun _ -> [WILDCARD])
        | ',' -> lex_action posn buf (fun _ -> [COMMA])
        | re_string -> lex_action posn buf (fun lb -> let s = Utf8.lexeme lb in [STRING (String.sub s 1 (String.length s - 2))])
        | '\\' -> lex_action posn buf (fun _ -> [LAMBDA])
        | '`' -> lex_action posn buf (fun _ -> [BACKTICK])
        | '.' -> lex_action posn buf (fun _ -> [DOT])
        | '|' -> lex_action posn buf (fun _ -> [BAR])
        | "[]" -> lex_action posn buf (fun _ -> [NIL])
        | '[' -> lex_action posn buf (fun _ -> [LBRACK])
        | ']' -> lex_action posn buf (fun _ -> [RBRACK])
        | ".." -> lex_action posn buf (fun _ -> [ELLIPSES])
        | Plus re_symbol -> lex_action posn buf (fun lb -> [OP (Utf8.lexeme lb)])
        | re_lident -> lex_action posn buf (fun lb -> [LOWERID (Utf8.lexeme lb)])
        | re_uident -> lex_action posn buf (fun lb -> [UPPERID (Utf8.lexeme lb)])
        | re_brk_char -> lex_action posn buf (fun lb -> match char_of_int (lexeme_char lb 2) with
            | 'n' -> [CHAR '\n']
            | 't' -> [CHAR '\t']
            | 'r' -> [CHAR '\r']
            | c -> [CHAR c])
        | re_char -> lex_action posn buf (fun lb -> [CHAR (char_of_int (lexeme_char lb 1))])
        | re_int -> lex_action posn buf (fun lb -> [INTEGER (int_of_string (Utf8.lexeme lb))])
        | re_float -> lex_action posn buf (fun lb -> [FLOATING (float_of_string (Utf8.lexeme lb))])
        | re_prim_brk_char -> lex_action posn buf (fun lb -> [PRIMCHAR (char_of_int (lexeme_char lb 2))])
        | re_prim_char -> lex_action posn buf (fun lb -> [PRIMCHAR (char_of_int (lexeme_char lb 1))])
        | re_prim_int -> lex_action posn buf (fun lb -> let s = Utf8.lexeme lb in [PRIMINTEGER (int_of_string (Util.strip_last_char s))])
        | re_prim_float -> lex_action posn buf (fun lb -> let s = Utf8.lexeme lb in [PRIMFLOATING (float_of_string (Util.strip_last_char s))])
        | Star white_space, eof ->
            let rec emit_dedents ts = match !lex_layout with
                | [] -> upd_posn posn buf, ts
                | _ :: xs -> lex_layout := xs; emit_dedents (DEDENT :: ts)
            in emit_dedents [EOF]
        | any -> failwith ("lexing error--unexpected character: " ^ Utf8.lexeme buf)
        | _ -> failwith "impossible"
