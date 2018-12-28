module rec Pattern : sig
    type 'a t =
        | Unit_pattern
        | Nil_pattern
        | Wildcard_pattern
        | Prim_int_pattern of int
        | Prim_char_pattern of char
        | String_pattern of string
        | Var_pattern of 'a
        | Alias_pattern of 'a t * 'a
        | Con_pattern of 'a * 'a t list
        | Or_pattern of 'a t * 'a t
        | View_pattern of 'a Exp.t * 'a t
        | Constraint_pattern of 'a t * 'a Type.t
end = struct
    type 'a t =
        | Unit_pattern
        | Nil_pattern
        | Wildcard_pattern
        | Prim_int_pattern of int
        | Prim_char_pattern of char
        | String_pattern of string
        | Var_pattern of 'a
        | Alias_pattern of 'a t * 'a
        | Con_pattern of 'a * 'a t list
        | Or_pattern of 'a t * 'a t
        | View_pattern of 'a Exp.t * 'a t
        | Constraint_pattern of 'a t * 'a Type.t
end
and Exp : sig
    type 'a t =
        | Unit
        | Int of int
        | Float of float
        | Char of char
        | String of string
        | Var of 'a
        | App of 'a t * 'a t
        | Infix of 'a * 'a t * 'a t
        | Lambda of 'a Pattern.t list * 'a t
        | Case_lambda of ('a Pattern.t * 'a t) list
        | Case of 'a t * ('a Pattern.t * 'a t) list
        | If of 'a t * 'a t * 'a t
        | While of 'a t * 'a t
        | Try_catch of 'a t * ('a Pattern.t * 'a t) list
        | Constraint of 'a t * 'a Type.t
        | Field_access of 'a t * 'a
        | Assignment of 'a t * 'a t
        | Let of 'a Binding.t list * 'a t
        | Seq of 'a t * 'a t
end = struct
    type 'a t =
        | Unit
        | Int of int
        | Float of float
        | Char of char
        | String of string
        | Var of 'a
        | App of 'a t * 'a t
        | Infix of 'a * 'a t * 'a t
        | Lambda of 'a Pattern.t list * 'a t
        | Case_lambda of ('a Pattern.t * 'a t) list
        | Case of 'a t * ('a Pattern.t * 'a t) list
        | If of 'a t * 'a t * 'a t
        | While of 'a t * 'a t
        | Try_catch of 'a t * ('a Pattern.t * 'a t) list
        | Constraint of 'a t * 'a Type.t
        | Field_access of 'a t * 'a
        | Assignment of 'a t * 'a t
        | Let of 'a Binding.t list * 'a t
        | Seq of 'a t * 'a t
end
and Binding : sig
    type 'a t =
        | Normal_binding of 'a * 'a Pattern.t list * 'a Exp.t option * 'a Exp.t * 'a t list
        | Pattern_binding of 'a Pattern.t * 'a Exp.t option * 'a Exp.t * 'a t list
end = struct
    type 'a t =
        | Normal_binding of 'a * 'a Pattern.t list * 'a Exp.t option * 'a Exp.t * 'a t list
        | Pattern_binding of 'a Pattern.t * 'a Exp.t option * 'a Exp.t * 'a t list
end

module Class = struct
    type 'a t =
        | Method_sig of 'a * 'a Type.t
        | Default_method of 'a Binding.t
end

module Top_level = struct
    type 'a t =
        | Infixl_decl of int * 'a
        | Infixr_decl of int * 'a
        | Infix_decl of int * 'a
        | Import_stmt of 'a
        | Data_defn of 'a * 'a list * ('a * 'a Type.t) list
        | Struct_defn of 'a * 'a list * ('a * 'a Type.t) list
        | Type_synonym of 'a * 'a list * 'a Type.t
        | Type_annot of 'a * 'a Type.t
        | Class_defn of ('a * 'a list) list * 'a * 'a list * 'a Class.t list
        | Inst_defn of ('a * 'a Type.t list) list * 'a * 'a Type.t list * 'a Binding.t list
        | Binding of 'a Binding.t
end

let pattern_match_failure = Exp.App (Exp.Var "panic#", Exp.Unit)
