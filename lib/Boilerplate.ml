(**
   Boilerplate to be used as a template when mapping the hack CST
   to another type of tree.
*)

(* Disable warnings against unused variables *)
[@@@warning "-26-27"]

(* Disable warning against unused 'rec' *)
[@@@warning "-39"]

type env = unit

let token (env : env) (_tok : Tree_sitter_run.Token.t) =
  failwith "not implemented"

let blank (env : env) () =
  failwith "not implemented"

let todo (env : env) _ =
   failwith "not implemented"

let map_type_modifier (env : env) (x : CST.type_modifier) =
  (match x with
  | `AT tok -> token env tok (* "@" *)
  | `QMARK tok -> token env tok (* "?" *)
  | `TILDE tok -> token env tok (* "~" *)
  )

let map_xhp_category_identifier (env : env) (tok : CST.xhp_category_identifier) =
  token env tok (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)

let map_heredoc_start (env : env) (tok : CST.heredoc_start) =
  token env tok (* heredoc_start *)

let map_identifier (env : env) (tok : CST.identifier) =
  token env tok (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)

let map_scope_identifier (env : env) (x : CST.scope_identifier) =
  (match x with
  | `Self tok -> token env tok (* "self" *)
  | `Parent tok -> token env tok (* "parent" *)
  | `Static tok -> token env tok (* "static" *)
  )

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  token env tok (* heredoc_end *)

let map_variable (env : env) (tok : CST.variable) =
  token env tok (* variable *)

let map_heredoc_body (env : env) (tok : CST.heredoc_body) =
  token env tok (* heredoc_body *)

let map_xhp_identifier (env : env) (tok : CST.xhp_identifier) =
  token env tok (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)

let map_integer (env : env) (tok : CST.integer) =
  token env tok (* integer *)

let map_float_ (env : env) (tok : CST.float_) =
  token env tok (* float *)

let map_pat_b6fe07e (env : env) (tok : CST.pat_b6fe07e) =
  token env tok (* pattern <\?[hH][hH] *)

let map_xhp_class_identifier (env : env) (tok : CST.xhp_class_identifier) =
  token env tok (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)

let map_null (env : env) (x : CST.null) =
  (match x with
  | `Null_37a6259 tok -> token env tok (* "null" *)
  | `Null_bbb93ef tok -> token env tok (* "Null" *)
  | `NULL tok -> token env tok (* "NULL" *)
  )

let map_string_ (env : env) (tok : CST.string_) =
  token env tok (* string *)

let map_xhp_string (env : env) (tok : CST.xhp_string) =
  token env tok (* pattern [^<]+ *)

let map_collection_type (env : env) (x : CST.collection_type) =
  (match x with
  | `Array tok -> token env tok (* "array" *)
  | `Varray tok -> token env tok (* "varray" *)
  | `Darray tok -> token env tok (* "darray" *)
  | `Vec tok -> token env tok (* "vec" *)
  | `Dict tok -> token env tok (* "dict" *)
  | `Keyset tok -> token env tok (* "keyset" *)
  )

let map_true_ (env : env) (x : CST.true_) =
  (match x with
  | `True_b326b50 tok -> token env tok (* "true" *)
  | `True_f827cf4 tok -> token env tok (* "True" *)
  | `TRUE tok -> token env tok (* "TRUE" *)
  )

let map_xhp_comment (env : env) (tok : CST.xhp_comment) =
  token env tok (* pattern <!--(.|[\n\r])*--> *)

let map_false_ (env : env) (x : CST.false_) =
  (match x with
  | `False_68934a3 tok -> token env tok (* "false" *)
  | `False_f8320b2 tok -> token env tok (* "False" *)
  | `FALSE tok -> token env tok (* "FALSE" *)
  )

let map_tok_LCURLDOLLAR_pat_0e8e4b6 (env : env) (tok : CST.tok_LCURLDOLLAR_pat_0e8e4b6) =
  token env tok (* tok_LCURLDOLLAR_pat_0e8e4b6 *)

let map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Public tok -> token env tok (* "public" *)
  | `Prot tok -> token env tok (* "protected" *)
  | `Priv tok -> token env tok (* "private" *)
  )

let map_use_type (env : env) (x : CST.use_type) =
  (match x with
  | `Name tok -> token env tok (* "namespace" *)
  | `Func tok -> token env tok (* "function" *)
  | `Type tok -> token env tok (* "type" *)
  | `Const tok -> token env tok (* "const" *)
  )

let map_anon_choice_QMARKDASHGT_ce9cc19 (env : env) (x : CST.anon_choice_QMARKDASHGT_ce9cc19) =
  (match x with
  | `QMARKDASHGT tok -> token env tok (* "?->" *)
  | `DASHGT tok -> token env tok (* "->" *)
  )

let map_xhp_category_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_category_declaration) =
  let v1 = token env v1 (* "category" *) in
  let v2 =
    token env v2 (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
  in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 =
        token env v2 (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
      in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

let map_xhp_close (env : env) ((v1, v2, v3) : CST.xhp_close) =
  let v1 = token env v1 (* "</" *) in
  let v2 =
    token env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
  in
  let v3 = token env v3 (* ">" *) in
  todo env (v1, v2, v3)

let rec map_xhp_attribute_expression (env : env) (x : CST.xhp_attribute_expression) =
  (match x with
  | `Xhp_id tok ->
      token env tok (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
  | `Xhp_class_id tok ->
      token env tok (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
  | `Xhp_cate_id tok ->
      token env tok (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
  | `Xhp_bin_exp (v1, v2, v3) ->
      let v1 = map_xhp_attribute_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_xhp_attribute_expression env v3 in
      todo env (v1, v2, v3)
  | `Xhp_post_un_exp (v1, v2) ->
      let v1 = map_xhp_attribute_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> token env tok (* "+" *)
        | `STAR tok -> token env tok (* "*" *)
        | `QMARK tok -> token env tok (* "?" *)
        )
      in
      todo env (v1, v2)
  | `Xhp_paren_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 = map_xhp_attribute_expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_xhp_attribute_expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  )

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Bool tok -> token env tok (* "bool" *)
  | `Float tok -> token env tok (* "float" *)
  | `Int tok -> token env tok (* "int" *)
  | `Str tok -> token env tok (* "string" *)
  | `Arra tok -> token env tok (* "arraykey" *)
  | `Void tok -> token env tok (* "void" *)
  | `Nonn tok -> token env tok (* "nonnull" *)
  | `Null x -> map_null env x
  | `Mixed tok -> token env tok (* "mixed" *)
  | `Dyna tok -> token env tok (* "dynamic" *)
  | `Nore tok -> token env tok (* "noreturn" *)
  )

let map_anon_choice_str_d42aa42 (env : env) (x : CST.anon_choice_str_d42aa42) =
  (match x with
  | `Str tok -> token env tok (* string *)
  | `Int tok -> token env tok (* integer *)
  )

let map_class_modifier (env : env) (x : CST.class_modifier) =
  (match x with
  | `Abst_modi tok -> token env tok (* "abstract" *)
  | `Final_modi tok -> token env tok (* "final" *)
  )

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Str tok -> token env tok (* string *)
  | `Int tok -> token env tok (* integer *)
  | `Float tok -> token env tok (* float *)
  | `True x -> map_true_ env x
  | `False x -> map_false_ env x
  | `Null x -> map_null env x
  )

let map_qualified_identifier (env : env) (x : CST.qualified_identifier) =
  (match x with
  | `Opt_id_rep1_back_id (v1, v2) ->
      let v1 =
        (match v1 with
        | Some tok ->
            token env tok (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
        | None -> todo env ())
      in
      let v2 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "\\" *) in
          let v2 =
            token env v2 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
          in
          todo env (v1, v2)
        ) v2
      in
      todo env (v1, v2)
  | `Id tok ->
      token env tok (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  )

let map_trait_alias_clause (env : env) ((v1, v2, v3) : CST.trait_alias_clause) =
  let v1 =
    token env v1 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  in
  let v2 = token env v2 (* "as" *) in
  let v3 =
    (match v3 with
    | `Visi_modi_opt_id (v1, v2) ->
        let v1 = map_visibility_modifier env v1 in
        let v2 =
          (match v2 with
          | Some tok ->
              token env tok (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
          | None -> todo env ())
        in
        todo env (v1, v2)
    | `Opt_visi_modi_id (v1, v2) ->
        let v1 =
          (match v1 with
          | Some x -> map_visibility_modifier env x
          | None -> todo env ())
        in
        let v2 =
          token env v2 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
        in
        todo env (v1, v2)
    )
  in
  todo env (v1, v2, v3)

let map_member_modifier (env : env) (x : CST.member_modifier) =
  (match x with
  | `Visi_modi x -> map_visibility_modifier env x
  | `Static_modi tok -> token env tok (* "static" *)
  | `Abst_modi tok -> token env tok (* "abstract" *)
  | `Final_modi tok -> token env tok (* "final" *)
  )

let map_xhp_children_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_children_declaration) =
  let v1 = token env v1 (* "children" *) in
  let v2 = map_xhp_attribute_expression env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_xhp_attribute_expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

let map_keyword (env : env) (x : CST.keyword) =
  (match x with
  | `Type tok -> token env tok (* "type" *)
  | `Newt tok -> token env tok (* "newtype" *)
  | `Shape tok -> token env tok (* "shape" *)
  | `Tupe tok -> token env tok (* "tupe" *)
  | `Choice_bool x -> map_primitive_type env x
  | `Choice_array x -> map_collection_type env x
  )

let map_xhp_enum_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.xhp_enum_type) =
  let v1 = token env v1 (* "enum" *) in
  let v2 = token env v2 (* "{" *) in
  let v3 = map_anon_choice_str_d42aa42 env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_anon_choice_str_d42aa42 env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v6 = token env v6 (* "}" *) in
  todo env (v1, v2, v3, v4, v5, v6)

let map_namespace_identifier (env : env) (x : CST.namespace_identifier) =
  (match x with
  | `Qual_id_opt_back (v1, v2) ->
      let v1 = map_qualified_identifier env v1 in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "\\" *)
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Back tok -> token env tok (* "\\" *)
  )

let rec map_type_constant_ (env : env) ((v1, v2, v3) : CST.type_constant_) =
  let v1 =
    (match v1 with
    | `Qual_id x -> map_qualified_identifier env x
    | `Type_cst_ x -> map_type_constant_ env x
    )
  in
  let v2 = token env v2 (* "::" *) in
  let v3 =
    token env v3 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  in
  todo env (v1, v2, v3)

let map_trait_select_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.trait_select_clause) =
  let v1 = map_qualified_identifier env v1 in
  let v2 = token env v2 (* "::" *) in
  let v3 =
    token env v3 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  in
  let v4 = token env v4 (* "insteadof" *) in
  let v5 = map_qualified_identifier env v5 in
  let v6 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_qualified_identifier env v2 in
      todo env (v1, v2)
    ) v6
  in
  todo env (v1, v2, v3, v4, v5, v6)

let map_anonymous_function_use_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_use_clause) =
  let v1 = token env v1 (* "use" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = token env v3 (* variable *) in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = token env v2 (* variable *) in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v6 = token env v6 (* ")" *) in
  todo env (v1, v2, v3, v4, v5, v6)

let map_scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) =
  let v1 =
    (match v1 with
    | `Qual_id x -> map_qualified_identifier env x
    | `Var tok -> token env tok (* variable *)
    | `Scope_id x -> map_scope_identifier env x
    | `Xhp_class_id tok ->
        token env tok (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
    | `Pipe_var tok -> token env tok (* "$$" *)
    )
  in
  let v2 = token env v2 (* "::" *) in
  let v3 =
    (match v3 with
    | `Id tok ->
        token env tok (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
    | `Var tok -> token env tok (* variable *)
    )
  in
  todo env (v1, v2, v3)

let map_anon_choice_id_0c70504 (env : env) (x : CST.anon_choice_id_0c70504) =
  (match x with
  | `Id tok ->
      token env tok (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  | `Kw x -> map_keyword env x
  )

let map_use_clause (env : env) ((v1, v2, v3) : CST.use_clause) =
  let v1 =
    (match v1 with
    | Some x -> map_use_type env x
    | None -> todo env ())
  in
  let v2 = map_namespace_identifier env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "as" *) in
        let v2 =
          token env v2 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let rec map_anon_choice_comp_stmt_c6c6bb4 (env : env) (x : CST.anon_choice_comp_stmt_c6c6bb4) =
  (match x with
  | `Comp_stmt x -> map_compound_statement env x
  | `SEMI tok -> token env tok (* ";" *)
  )

and map_anon_choice_exp_1701d0a (env : env) (x : CST.anon_choice_exp_1701d0a) =
  (match x with
  | `Exp x -> map_expression env x
  | `Elem_init (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "=>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb (env : env) ((v1, v2, v3) : CST.anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb) =
  let v1 = map_anon_choice_exp_1701d0a env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_anon_choice_exp_1701d0a env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_anon_choice_field_spec_0e0e023 (env : env) (x : CST.anon_choice_field_spec_0e0e023) =
  (match x with
  | `Field_spec (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "?" *)
        | None -> todo env ())
      in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* "=>" *) in
      let v4 = map_type_ env v4 in
      todo env (v1, v2, v3, v4)
  | `DOTDOTDOT tok -> token env tok (* "..." *)
  )

and map_anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_argument (env : env) ((v1, v2) : CST.argument) =
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Inout_modi tok -> token env tok (* "inout" *)
        | `Vari_modi tok -> token env tok (* "..." *)
        )
    | None -> todo env ())
  in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_argument env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_argument env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_attribute_modifier (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_modifier) =
  let v1 = token env v1 (* "<<" *) in
  let v2 = map_qualified_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_arguments env x
    | None -> todo env ())
  in
  let v4 =
    List.map (fun (v1, v2, v3) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_qualified_identifier env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_arguments env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v6 = token env v6 (* ">>" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_BARGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "??" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "||" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "==" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "===" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "!==" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<=>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<<" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "+" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "-" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "." *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "*" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "/" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "%" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "**" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKCOLON_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "?:" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKQMARKEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "??=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DOTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ".=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAREQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "|=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HATEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "^=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "&=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "<<=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* ">>=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUSEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "+=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASHEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "-=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAREQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "*=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASHEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "/=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERCEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "%=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAREQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "**=" *) in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_call_expression (env : env) ((v1, v2, v3) : CST.call_expression) =
  let v1 =
    (match v1 with
    | `Exp x -> map_expression env x
    | `Choice_array x -> map_collection_type env x
    )
  in
  let v2 =
    (match v2 with
    | Some x -> map_type_arguments env x
    | None -> todo env ())
  in
  let v3 = map_arguments env v3 in
  todo env (v1, v2, v3)

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) =
  let v1 = token env v1 (* "catch" *) in
  let v2 = token env v2 (* "(" *) in
  let v3 = map_type_ env v3 in
  let v4 = token env v4 (* variable *) in
  let v5 = token env v5 (* ")" *) in
  let v6 = map_compound_statement env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_class_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_const_declaration) =
  let v1 = List.map (map_member_modifier env) v1 in
  let v2 = token env v2 (* "const" *) in
  let v3 =
    (match v3 with
    | Some x -> map_type_ env x
    | None -> todo env ())
  in
  let v4 = map_class_const_declarator env v4 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_class_const_declarator env v2 in
      todo env (v1, v2)
    ) v5
  in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_class_const_declarator (env : env) ((v1, v2) : CST.class_const_declarator) =
  let v1 = map_anon_choice_id_0c70504 env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = token env v1 (* "{" *) in
  let v2 = List.map (map_statement env) v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_const_declarator (env : env) ((v1, v2, v3) : CST.const_declarator) =
  let v1 = map_anon_choice_id_0c70504 env v1 in
  let v2 = token env v2 (* "=" *) in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_declaration (env : env) (x : CST.declaration) =
  (match x with
  | `Func_decl (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = map_function_declaration_header env v2 in
      let v3 = map_anon_choice_comp_stmt_c6c6bb4 env v3 in
      todo env (v1, v2, v3)
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some x -> map_class_modifier env x
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | Some x -> map_class_modifier env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "class" *) in
      let v5 =
        (match v5 with
        | `Id tok ->
            token env tok (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
        | `Xhp_class_id tok ->
            token env tok (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
        )
      in
      let v6 =
        (match v6 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v7 =
        (match v7 with
        | Some x -> map_extends_clause env x
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> map_implements_clause env x
        | None -> todo env ())
      in
      let v9 =
        (match v9 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v10 = map_member_declarations env v10 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)
  | `Inte_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "interface" *) in
      let v3 =
        token env v3 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
      in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_extends_clause env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v7 = map_member_declarations env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Trait_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "trait" *) in
      let v3 =
        token env v3 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
      in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_implements_clause env x
        | None -> todo env ())
      in
      let v6 =
        (match v6 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v7 = map_member_declarations env v7 in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Alias_decl (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | `Type tok -> token env tok (* "type" *)
        | `Newt tok -> token env tok (* "newtype" *)
        )
      in
      let v3 =
        token env v3 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
      in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "as" *) in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v6 = token env v6 (* "=" *) in
      let v7 = map_type_ env v7 in
      let v8 = token env v8 (* ";" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Enum_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = token env v2 (* "enum" *) in
      let v3 =
        token env v3 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
      in
      let v4 = token env v4 (* ":" *) in
      let v5 = map_type_ env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "as" *) in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 = token env v7 (* "{" *) in
      let v8 = List.map (map_enumerator env) v8 in
      let v9 = token env v9 (* "}" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Name_decl (v1, v2) ->
      let v1 = token env v1 (* "namespace" *) in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Qual_id_SEMI (v1, v2) ->
                let v1 = map_qualified_identifier env v1 in
                let v2 = token env v2 (* ";" *) in
                todo env (v1, v2)
            | `Opt_qual_id_comp_stmt (v1, v2) ->
                let v1 =
                  (match v1 with
                  | Some x -> map_qualified_identifier env x
                  | None -> todo env ())
                in
                let v2 = map_compound_statement env v2 in
                todo env (v1, v2)
            )
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Const_decl (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "const" *) in
      let v2 =
        (match v2 with
        | Some x -> map_type_ env x
        | None -> todo env ())
      in
      let v3 = map_const_declarator env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_const_declarator env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 = token env v5 (* ";" *) in
      todo env (v1, v2, v3, v4, v5)
  )

and map_embedded_brace_expression (env : env) ((v1, v2) : CST.embedded_brace_expression) =
  let v1 = map_embedded_brace_expression_ env v1 in
  let v2 = token env v2 (* "}" *) in
  todo env (v1, v2)

and map_embedded_brace_expression_ (env : env) (x : CST.embedded_brace_expression_) =
  (match x with
  | `Tok_LCURLDOLLAR_pat_0e8e4b6 tok ->
      token env tok (* tok_LCURLDOLLAR_pat_0e8e4b6 *)
  | `Embe_brace_call_exp (v1, v2) ->
      let v1 = map_embedded_brace_expression_ env v1 in
      let v2 = map_arguments env v2 in
      todo env (v1, v2)
  | `Embe_brace_subs_exp (v1, v2, v3, v4) ->
      let v1 = map_embedded_brace_expression_ env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Embe_brace_sele_exp (v1, v2, v3) ->
      let v1 = map_embedded_brace_expression_ env v1 in
      let v2 = map_anon_choice_QMARKDASHGT_ce9cc19 env v2 in
      let v3 = map_variablish env v3 in
      todo env (v1, v2, v3)
  )

and map_enumerator (env : env) ((v1, v2, v3, v4) : CST.enumerator) =
  let v1 =
    token env v1 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  in
  let v2 = token env v2 (* "=" *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Here (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "<<<" *) in
      let v2 = token env v2 (* heredoc_start *) in
      let v3 =
        List.map (fun x ->
          (match x with
          | `Here_body tok -> token env tok (* heredoc_body *)
          | `Var tok -> token env tok (* variable *)
          | `Embe_brace_exp x -> map_embedded_brace_expression env x
          )
        ) v3
      in
      let v4 = token env v4 (* heredoc_end *) in
      todo env (v1, v2, v3, v4)
  | `Array (v1, v2, v3, v4, v5) ->
      let v1 = map_collection_type env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* "[" *) in
      let v4 =
        (match v4 with
        | Some x ->
            map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
        | None -> todo env ())
      in
      let v5 = token env v5 (* "]" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Tuple (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "tuple" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) ->
            let v1 = map_expression env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_expression env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> token env tok (* "," *)
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Shape (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "shape" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some (v1, v2, v3) ->
            let v1 = map_field_initializer env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_field_initializer env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> token env tok (* "," *)
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      let v4 = token env v4 (* ")" *) in
      todo env (v1, v2, v3, v4)
  | `Coll (v1, v2, v3, v4) ->
      let v1 = map_qualified_identifier env v1 in
      let v2 = token env v2 (* "{" *) in
      let v3 =
        (match v3 with
        | Some x ->
            map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "}" *) in
      todo env (v1, v2, v3, v4)
  | `Choice_str x -> map_literal env x
  | `Choice_var x -> map_variablish env x
  | `Pref_str (v1, v2) ->
      let v1 =
        token env v1 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
      in
      let v2 = token env v2 (* string *) in
      todo env (v1, v2)
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Bin_exp x -> map_binary_expression env x
  | `Prefix_un_exp x -> map_prefix_unary_expression env x
  | `Post_un_exp (v1, v2) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUSPLUS tok -> token env tok (* "++" *)
        | `DASHDASH tok -> token env tok (* "--" *)
        )
      in
      todo env (v1, v2)
  | `Is_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "is" *) in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `As_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 =
        (match v2 with
        | `As tok -> token env tok (* as *)
        | `QMARKas tok -> token env tok (* "?as" *)
        )
      in
      let v3 = map_type_ env v3 in
      todo env (v1, v2, v3)
  | `Awai_exp (v1, v2) ->
      let v1 = token env v1 (* "async" *) in
      let v2 = map_compound_statement env v2 in
      todo env (v1, v2)
  | `Yield_exp (v1, v2) ->
      let v1 = token env v1 (* "yield" *) in
      let v2 = map_anon_choice_exp_1701d0a env v2 in
      todo env (v1, v2)
  | `Cast_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "(" *) in
      let v2 =
        (match v2 with
        | `Array tok -> token env tok (* "array" *)
        | `Int tok -> token env tok (* "int" *)
        | `Float tok -> token env tok (* "float" *)
        | `Str tok -> token env tok (* "string" *)
        | `Bool tok -> token env tok (* "bool" *)
        )
      in
      let v3 = token env v3 (* ")" *) in
      let v4 = map_expression env v4 in
      todo env (v1, v2, v3, v4)
  | `Tern_exp (v1, v2, v3, v4, v5) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "?" *) in
      let v3 = map_expression env v3 in
      let v4 = token env v4 (* ":" *) in
      let v5 = map_expression env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Lambda_exp (v1, v2, v3, v4, v5) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "async" *)
        | None -> todo env ())
      in
      let v3 =
        (match v3 with
        | `Single_param_params tok -> token env tok (* variable *)
        | `Params_opt_COLON_choice_type_spec (v1, v2) ->
            let v1 = map_parameters env v1 in
            let v2 =
              (match v2 with
              | Some (v1, v2) ->
                  let v1 = token env v1 (* ":" *) in
                  let v2 = map_type_ env v2 in
                  todo env (v1, v2)
              | None -> todo env ())
            in
            todo env (v1, v2)
        )
      in
      let v4 = token env v4 (* "==>" *) in
      let v5 =
        (match v5 with
        | `Exp x -> map_expression env x
        | `Comp_stmt x -> map_compound_statement env x
        )
      in
      todo env (v1, v2, v3, v4, v5)
  | `Call_exp x -> map_call_expression env x
  | `Sele_exp x -> map_selection_expression env x
  | `New_exp (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "new" *) in
      let v2 = map_variablish env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      let v4 = map_arguments env v4 in
      todo env (v1, v2, v3, v4)
  | `Incl_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Incl tok -> token env tok (* "include" *)
        | `Incl_once tok -> token env tok (* "include_once" *)
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Requ_exp (v1, v2) ->
      let v1 =
        (match v1 with
        | `Requ tok -> token env tok (* "require" *)
        | `Requ_once tok -> token env tok (* "require_once" *)
        )
      in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Anon_func_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "async" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "function" *) in
      let v3 = map_parameters env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* ":" *) in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some x -> map_anonymous_function_use_clause env x
        | None -> todo env ())
      in
      let v6 = map_compound_statement env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Xhp_exp x -> map_xhp_expression env x
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expression env v1 in
  let v2 = token env v2 (* ";" *) in
  todo env (v1, v2)

and map_extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = token env v1 (* "extends" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  let v1 =
    (match v1 with
    | `Str tok -> token env tok (* string *)
    | `Scoped_id x -> map_scoped_identifier env x
    )
  in
  let v2 = token env v2 (* "=>" *) in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = token env v1 (* "finally" *) in
  let v2 = map_compound_statement env v2 in
  todo env (v1, v2)

and map_function_declaration_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_declaration_header) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* "async" *)
    | None -> todo env ())
  in
  let v2 = token env v2 (* "function" *) in
  let v3 =
    token env v3 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v5 = map_parameters env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2, v3) ->
        let v1 = token env v1 (* ":" *) in
        let v2 =
          (match v2 with
          | Some x -> map_attribute_modifier env x
          | None -> todo env ())
        in
        let v3 = map_type_ env v3 in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v7 =
    (match v7 with
    | Some x -> map_where_clause env x
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let v1 = token env v1 (* "implements" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_member_declarations (env : env) ((v1, v2, v3) : CST.member_declarations) =
  let v1 = token env v1 (* "{" *) in
  let v2 =
    List.map (fun x ->
      (match x with
      | `Class_const_decl x -> map_class_const_declaration env x
      | `Meth_decl x -> map_method_declaration env x
      | `Prop_decl x -> map_property_declaration env x
      | `Type_const_decl x -> map_type_const_declaration env x
      | `Trait_use_clause x -> map_trait_use_clause env x
      | `Requ_imples_clause x ->
          map_require_implements_clause env x
      | `Requ_extends_clause x -> map_require_extends_clause env x
      | `Xhp_attr_decl x -> map_xhp_attribute_declaration env x
      | `Xhp_chil_decl x -> map_xhp_children_declaration env x
      | `Xhp_cate_decl x -> map_xhp_category_declaration env x
      )
    ) v2
  in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_method_declaration (env : env) ((v1, v2, v3, v4) : CST.method_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_attribute_modifier env x
    | None -> todo env ())
  in
  let v2 = List.map (map_member_modifier env) v2 in
  let v3 = map_function_declaration_header env v3 in
  let v4 = map_anon_choice_comp_stmt_c6c6bb4 env v4 in
  todo env (v1, v2, v3, v4)

and map_parameter (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.parameter) =
  let v1 =
    (match v1 with
    | Some x -> map_attribute_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some x -> map_visibility_modifier env x
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some tok -> token env tok (* "inout" *)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x -> map_type_ env x
    | None -> todo env ())
  in
  let v5 =
    (match v5 with
    | Some tok -> token env tok (* "..." *)
    | None -> todo env ())
  in
  let v6 = token env v6 (* variable *) in
  let v7 =
    (match v7 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4, v5, v6, v7)

and map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = token env v1 (* "(" *) in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Vari_modi tok -> token env tok (* "..." *)
        | `Param_rep_COMMA_param_opt_COMMA (v1, v2, v3) ->
            let v1 = map_parameter env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_parameter env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> token env tok (* "," *)
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        )
    | None -> todo env ())
  in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = token env v1 (* "(" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* ")" *) in
  todo env (v1, v2, v3)

and map_prefix_unary_expression (env : env) (x : CST.prefix_unary_expression) =
  (match x with
  | `BANG_exp (v1, v2) ->
      let v1 = token env v1 (* "!" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = token env v1 (* "~" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `DASH_exp (v1, v2) ->
      let v1 = token env v1 (* "-" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `PLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "+" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = token env v1 (* "++" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `DASHDASH_exp (v1, v2) ->
      let v1 = token env v1 (* "--" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Print_exp (v1, v2) ->
      let v1 = token env v1 (* "print" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Clone_exp (v1, v2) ->
      let v1 = token env v1 (* "clone" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Await_exp (v1, v2) ->
      let v1 = token env v1 (* "await" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `AT_exp (v1, v2) ->
      let v1 = token env v1 (* "@" *) in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  )

and map_property_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.property_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_attribute_modifier env x
    | None -> todo env ())
  in
  let v2 = List.map (map_member_modifier env) v2 in
  let v3 =
    (match v3 with
    | Some x -> map_type_ env x
    | None -> todo env ())
  in
  let v4 = map_property_declarator env v4 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_property_declarator env v2 in
      todo env (v1, v2)
    ) v5
  in
  let v6 = token env v6 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6)

and map_property_declarator (env : env) ((v1, v2) : CST.property_declarator) =
  let v1 = token env v1 (* variable *) in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_require_extends_clause (env : env) ((v1, v2, v3, v4, v5) : CST.require_extends_clause) =
  let v1 = token env v1 (* "require" *) in
  let v2 = token env v2 (* "extends" *) in
  let v3 = map_type_ env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 = token env v5 (* ";" *) in
  todo env (v1, v2, v3, v4, v5)

and map_require_implements_clause (env : env) ((v1, v2, v3, v4, v5) : CST.require_implements_clause) =
  let v1 = token env v1 (* "require" *) in
  let v2 = token env v2 (* "implements" *) in
  let v3 = map_type_ env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 = token env v5 (* ";" *) in
  todo env (v1, v2, v3, v4, v5)

and map_selection_expression (env : env) ((v1, v2, v3) : CST.selection_expression) =
  let v1 = map_variablish env v1 in
  let v2 = map_anon_choice_QMARKDASHGT_ce9cc19 env v2 in
  let v3 = map_variablish env v3 in
  todo env (v1, v2, v3)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_func_decl x -> map_declaration env x
  | `Comp_stmt x -> map_compound_statement env x
  | `Empty_stmt tok -> token env tok (* ";" *)
  | `Exp_stmt x -> map_expression_statement env x
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "return" *) in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "break" *) in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "continue" *) in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "throw" *) in
      let v2 = map_expression env v2 in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `Echo_stmt (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "echo" *) in
      let v2 = map_expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = token env v4 (* ";" *) in
      todo env (v1, v2, v3, v4)
  | `Unset_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "unset" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = map_variablish env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_variablish env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v4 = token env v4 (* ")" *) in
      let v5 = token env v5 (* ";" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Use_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "use" *) in
      let v2 =
        (match v2 with
        | `Use_clause_rep_COMMA_use_clause_opt_COMMA (v1, v2, v3) ->
            let v1 = map_use_clause env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_use_clause env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> token env tok (* "," *)
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | `Opt_use_type_name_id_LCURL_use_clause_rep_COMMA_use_clause_opt_COMMA_RCURL (v1, v2, v3, v4, v5, v6, v7) ->
            let v1 =
              (match v1 with
              | Some x -> map_use_type env x
              | None -> todo env ())
            in
            let v2 = map_namespace_identifier env v2 in
            let v3 = token env v3 (* "{" *) in
            let v4 = map_use_clause env v4 in
            let v5 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_use_clause env v2 in
                todo env (v1, v2)
              ) v5
            in
            let v6 =
              (match v6 with
              | Some tok -> token env tok (* "," *)
              | None -> todo env ())
            in
            let v7 = token env v7 (* "}" *) in
            todo env (v1, v2, v3, v4, v5, v6, v7)
        )
      in
      let v3 = token env v3 (* ";" *) in
      todo env (v1, v2, v3)
  | `If_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "if" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      let v4 =
        List.map (fun (v1, v2, v3) ->
          let v1 =
            (match v1 with
            | `Elseif tok -> token env tok (* "elseif" *)
            | `Else_if (v1, v2) ->
                let v1 = token env v1 (* "else" *) in
                let v2 = token env v2 (* "if" *) in
                todo env (v1, v2)
            )
          in
          let v2 = map_parenthesized_expression env v2 in
          let v3 = map_statement env v3 in
          todo env (v1, v2, v3)
        ) v4
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = token env v1 (* "else" *) in
            let v2 = map_statement env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  | `While_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "while" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "do" *) in
      let v2 = map_statement env v2 in
      let v3 = token env v3 (* "while" *) in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = token env v5 (* ";" *) in
      todo env (v1, v2, v3, v4, v5)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = token env v1 (* "for" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* ";" *) in
      let v5 =
        (match v5 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v6 = token env v6 (* ";" *) in
      let v7 =
        (match v7 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v8 = token env v8 (* ")" *) in
      let v9 = map_statement env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Switch_stmt (v1, v2, v3, v4, v5) ->
      let v1 = token env v1 (* "switch" *) in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = token env v3 (* "{" *) in
      let v4 =
        List.map (fun x ->
          (match x with
          | `Switch_case x -> map_switch_case env x
          | `Switch_defa x -> map_switch_default env x
          )
        ) v4
      in
      let v5 = token env v5 (* "}" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = token env v1 (* "foreach" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_expression env v3 in
      let v4 =
        (match v4 with
        | Some tok -> token env tok (* "await" *)
        | None -> todo env ())
      in
      let v5 = token env v5 (* as *) in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = map_variablish env v1 in
            let v2 = token env v2 (* "=>" *) in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 = map_variablish env v7 in
      let v8 = token env v8 (* ")" *) in
      let v9 = map_statement env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Try_stmt (v1, v2, v3) ->
      let v1 = token env v1 (* "try" *) in
      let v2 = map_compound_statement env v2 in
      let v3 =
        List.map (fun x ->
          (match x with
          | `Catch_clause x -> map_catch_clause env x
          | `Fina_clause x -> map_finally_clause env x
          )
        ) v3
      in
      todo env (v1, v2, v3)
  | `Conc_stmt (v1, v2) ->
      let v1 = token env v1 (* "concurrent" *) in
      let v2 = map_compound_statement env v2 in
      todo env (v1, v2)
  | `Using_stmt (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some tok -> token env tok (* "await" *)
        | None -> todo env ())
      in
      let v2 = token env v2 (* "using" *) in
      let v3 =
        (match v3 with
        | `Exp_stmt x -> map_expression_statement env x
        | `LPAR_exp_rep_COMMA_exp_RPAR_choice_comp_stmt (v1, v2, v3, v4, v5) ->
            let v1 = token env v1 (* "(" *) in
            let v2 = map_expression env v2 in
            let v3 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_expression env v2 in
                todo env (v1, v2)
              ) v3
            in
            let v4 = token env v4 (* ")" *) in
            let v5 = map_anon_choice_comp_stmt_c6c6bb4 env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      todo env (v1, v2, v3)
  )

and map_switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = token env v1 (* "case" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* ":" *) in
  let v4 = List.map (map_statement env) v4 in
  todo env (v1, v2, v3, v4)

and map_switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = token env v1 (* "default" *) in
  let v2 = token env v2 (* ":" *) in
  let v3 = List.map (map_statement env) v3 in
  todo env (v1, v2, v3)

and map_trait_use_clause (env : env) ((v1, v2, v3, v4) : CST.trait_use_clause) =
  let v1 = token env v1 (* "use" *) in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | `LCURL_rep_choice_trait_select_clause_SEMI_RCURL (v1, v2, v3) ->
        let v1 = token env v1 (* "{" *) in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `Trait_select_clause x -> map_trait_select_clause env x
              | `Trait_alias_clause x -> map_trait_alias_clause env x
              )
            in
            let v2 = token env v2 (* ";" *) in
            todo env (v1, v2)
          ) v2
        in
        let v3 = token env v3 (* "}" *) in
        todo env (v1, v2, v3)
    | `SEMI tok -> token env tok (* ";" *)
    )
  in
  todo env (v1, v2, v3, v4)

and map_type_ (env : env) (x : CST.type_) =
  (match x with
  | `Type_spec (v1, v2, v3) ->
      let v1 = List.map (map_type_modifier env) v1 in
      let v2 =
        (match v2 with
        | `Choice_bool x -> map_primitive_type env x
        | `Qual_id x -> map_qualified_identifier env x
        | `Choice_array x -> map_collection_type env x
        | `Xhp_class_id tok ->
            token env tok (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
        )
      in
      let v3 =
        (match v3 with
        | Some x -> map_type_arguments env x
        | None -> todo env ())
      in
      todo env (v1, v2, v3)
  | `Type_cst (v1, v2) ->
      let v1 = List.map (map_type_modifier env) v1 in
      let v2 = map_type_constant_ env v2 in
      todo env (v1, v2)
  | `Shape_type_spec (v1, v2, v3, v4, v5) ->
      let v1 = List.map (map_type_modifier env) v1 in
      let v2 = token env v2 (* "shape" *) in
      let v3 = token env v3 (* "(" *) in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3) ->
            let v1 = map_anon_choice_field_spec_0e0e023 env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = token env v1 (* "," *) in
                let v2 = map_anon_choice_field_spec_0e0e023 env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> token env tok (* "," *)
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      let v5 = token env v5 (* ")" *) in
      todo env (v1, v2, v3, v4, v5)
  | `Func_type_spec (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = List.map (map_type_modifier env) v1 in
      let v2 = token env v2 (* "(" *) in
      let v3 = token env v3 (* "function" *) in
      let v4 = token env v4 (* "(" *) in
      let v5 =
        (match v5 with
        | Some (v1, v2, v3, v4, v5) ->
            let v1 =
              (match v1 with
              | Some tok -> token env tok (* "inout" *)
              | None -> todo env ())
            in
            let v2 = map_type_ env v2 in
            let v3 =
              (match v3 with
              | Some tok -> token env tok (* "..." *)
              | None -> todo env ())
            in
            let v4 =
              List.map (fun (v1, v2, v3, v4) ->
                let v1 = token env v1 (* "," *) in
                let v2 =
                  (match v2 with
                  | Some tok -> token env tok (* "inout" *)
                  | None -> todo env ())
                in
                let v3 = map_type_ env v3 in
                let v4 =
                  (match v4 with
                  | Some tok -> token env tok (* "..." *)
                  | None -> todo env ())
                in
                todo env (v1, v2, v3, v4)
              ) v4
            in
            let v5 =
              (match v5 with
              | Some tok -> token env tok (* "," *)
              | None -> todo env ())
            in
            todo env (v1, v2, v3, v4, v5)
        | None -> todo env ())
      in
      let v6 = token env v6 (* ")" *) in
      let v7 = token env v7 (* ":" *) in
      let v8 = map_type_ env v8 in
      let v9 = token env v9 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Tuple_type_spec (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.map (map_type_modifier env) v1 in
      let v2 = token env v2 (* "(" *) in
      let v3 = map_type_ env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 = map_type_ env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 =
        (match v5 with
        | Some tok -> token env tok (* "," *)
        | None -> todo env ())
      in
      let v6 = token env v6 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  )

and map_type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_type_ env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = token env v1 (* "," *) in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> token env tok (* "," *)
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = token env v3 (* ">" *) in
  todo env (v1, v2, v3)

and map_type_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.type_const_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_attribute_modifier env x
    | None -> todo env ())
  in
  let v2 = List.map (map_member_modifier env) v2 in
  let v3 = token env v3 (* "const" *) in
  let v4 = token env v4 (* "type" *) in
  let v5 =
    token env v5 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  in
  let v6 =
    (match v6 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v7 =
    (match v7 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "as" *) in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v8 =
    (match v8 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v9 = token env v9 (* ";" *) in
  todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)

and map_type_parameter (env : env) ((v1, v2, v3, v4) : CST.type_parameter) =
  let v1 =
    (match v1 with
    | Some x -> map_attribute_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `PLUS tok -> token env tok (* "+" *)
        | `DASH tok -> token env tok (* "-" *)
        | `Reify tok -> token env tok (* "reify" *)
        )
    | None -> todo env ())
  in
  let v3 =
    token env v3 (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *)
  in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `As tok -> token env tok (* "as" *)
        | `Super tok -> token env tok (* "super" *)
        )
      in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  todo env (v1, v2, v3, v4)

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = token env v1 (* "<" *) in
  let v2 = map_type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> token env tok (* "," *)
    | None -> todo env ())
  in
  let v5 = token env v5 (* ">" *) in
  todo env (v1, v2, v3, v4, v5)

and map_variablish (env : env) (x : CST.variablish) =
  (match x with
  | `Var tok -> token env tok (* variable *)
  | `Pipe_var tok -> token env tok (* "$$" *)
  | `List_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = token env v1 (* "list" *) in
      let v2 = token env v2 (* "(" *) in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = token env v1 (* "," *) in
          let v2 =
            (match v2 with
            | Some x -> map_expression env x
            | None -> todo env ())
          in
          todo env (v1, v2)
        ) v4
      in
      let v5 =
        (match v5 with
        | Some tok -> token env tok (* "," *)
        | None -> todo env ())
      in
      let v6 = token env v6 (* ")" *) in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Subs_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = token env v2 (* "[" *) in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 = token env v4 (* "]" *) in
      todo env (v1, v2, v3, v4)
  | `Qual_id x -> map_qualified_identifier env x
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Call_exp x -> map_call_expression env x
  | `Scoped_id x -> map_scoped_identifier env x
  | `Scope_id x -> map_scope_identifier env x
  | `Sele_exp x -> map_selection_expression env x
  | `Xhp_class_id tok ->
      token env tok (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
  )

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = token env v1 (* "where" *) in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = map_where_constraint env v1 in
      let v2 =
        (match v2 with
        | Some tok -> token env tok (* "," *)
        | None -> todo env ())
      in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_where_constraint (env : env) ((v1, v2, v3) : CST.where_constraint) =
  let v1 = map_type_ env v1 in
  let v2 =
    (match v2 with
    | `As tok -> token env tok (* "as" *)
    | `Super tok -> token env tok (* "super" *)
    | `EQ tok -> token env tok (* "=" *)
    )
  in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_xhp_attribute (env : env) (x : CST.xhp_attribute) =
  (match x with
  | `Xhp_id_EQ_choice_str (v1, v2, v3) ->
      let v1 =
        token env v1 (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
      in
      let v2 = token env v2 (* "=" *) in
      let v3 =
        (match v3 with
        | `Str tok -> token env tok (* string *)
        | `Xhp_braced_exp x -> map_xhp_braced_expression env x
        )
      in
      todo env (v1, v2, v3)
  | `Choice_xhp_braced_exp x ->
      (match x with
      | `Xhp_braced_exp x -> map_xhp_braced_expression env x
      | `Xhp_spread_exp x -> map_xhp_spread_expression env x
      )
  )

and map_xhp_attribute_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_attribute_declaration) =
  let v1 = token env v1 (* "attribute" *) in
  let v2 = map_xhp_class_attribute env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = token env v1 (* "," *) in
      let v2 = map_xhp_class_attribute env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = token env v4 (* ";" *) in
  todo env (v1, v2, v3, v4)

and map_xhp_braced_expression (env : env) ((v1, v2, v3) : CST.xhp_braced_expression) =
  let v1 = token env v1 (* "{" *) in
  let v2 = map_expression env v2 in
  let v3 = token env v3 (* "}" *) in
  todo env (v1, v2, v3)

and map_xhp_class_attribute (env : env) ((v1, v2, v3, v4) : CST.xhp_class_attribute) =
  let v1 =
    (match v1 with
    | `Choice_type_spec x -> map_type_ env x
    | `Xhp_enum_type x -> map_xhp_enum_type env x
    )
  in
  let v2 =
    (match v2 with
    | Some tok ->
        token env tok (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = token env v1 (* "=" *) in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `ATre tok -> token env tok (* "@required" *)
        | `ATla tok -> token env tok (* "@lateinit" *)
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_xhp_expression (env : env) (x : CST.xhp_expression) =
  (match x with
  | `Xhp_open_close (v1, v2, v3, v4) ->
      let v1 = token env v1 (* "<" *) in
      let v2 =
        token env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
      in
      let v3 = List.map (map_xhp_attribute env) v3 in
      let v4 = token env v4 (* "/>" *) in
      todo env (v1, v2, v3, v4)
  | `Xhp_open_rep_choice_xhp_str_xhp_close (v1, v2, v3) ->
      let v1 = map_xhp_open env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Xhp_str tok -> token env tok (* pattern [^<]+ *)
          | `Xhp_comm tok ->
              token env tok (* pattern <!--(.|[\n\r])*--> *)
          | `Xhp_braced_exp x -> map_xhp_braced_expression env x
          | `Xhp_exp x -> map_xhp_expression env x
          )
        ) v2
      in
      let v3 = map_xhp_close env v3 in
      todo env (v1, v2, v3)
  )

and map_xhp_open (env : env) ((v1, v2, v3, v4) : CST.xhp_open) =
  let v1 = token env v1 (* "<" *) in
  let v2 =
    token env v2 (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *)
  in
  let v3 = List.map (map_xhp_attribute env) v3 in
  let v4 = token env v4 (* ">" *) in
  todo env (v1, v2, v3, v4)

and map_xhp_spread_expression (env : env) ((v1, v2, v3, v4) : CST.xhp_spread_expression) =
  let v1 = token env v1 (* "{" *) in
  let v2 = token env v2 (* "..." *) in
  let v3 = map_expression env v3 in
  let v4 = token env v4 (* "}" *) in
  todo env (v1, v2, v3, v4)

let map_script (env : env) ((v1, v2) : CST.script) =
  let v1 =
    (match v1 with
    | Some tok -> token env tok (* pattern <\?[hH][hH] *)
    | None -> todo env ())
  in
  let v2 = List.map (map_statement env) v2 in
  todo env (v1, v2)