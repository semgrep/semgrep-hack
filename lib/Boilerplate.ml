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

let map_heredoc_end (env : env) (tok : CST.heredoc_end) =
  (* heredoc_end *) token env tok

let map_heredoc_end_newline (env : env) (tok : CST.heredoc_end_newline) =
  (* heredoc_end_newline *) token env tok

let map_heredoc_body (env : env) (tok : CST.heredoc_body) =
  (* heredoc_body *) token env tok

let map_semgrep_identifier (env : env) (tok : CST.semgrep_identifier) =
  (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok

let map_variable (env : env) (tok : CST.variable) =
  (* variable *) token env tok

let map_pat_466b599 (env : env) (tok : CST.pat_466b599) =
  (* pattern function\s*\( *) token env tok

let map_xhp_class_identifier (env : env) (tok : CST.xhp_class_identifier) =
  (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok

let map_xhp_category_identifier (env : env) (tok : CST.xhp_category_identifier) =
  (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok

let map_false_ (env : env) (x : CST.false_) =
  (match x with
  | `False_68934a3 tok -> (* "false" *) token env tok
  | `False_f8320b2 tok -> (* "False" *) token env tok
  | `FALSE tok -> (* "FALSE" *) token env tok
  )

let map_pat_b6fe07e (env : env) (tok : CST.pat_b6fe07e) =
  (* pattern <\?[hH][hH] *) token env tok

let map_xhp_string (env : env) (tok : CST.xhp_string) =
  (* xhp_string *) token env tok

let map_true_ (env : env) (x : CST.true_) =
  (match x with
  | `True_b326b50 tok -> (* "true" *) token env tok
  | `True_f827cf4 tok -> (* "True" *) token env tok
  | `TRUE tok -> (* "TRUE" *) token env tok
  )

let map_identifier (env : env) (tok : CST.identifier) =
  (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok

let map_tok_lcurldollar_pat_0e8e4b6 (env : env) (tok : CST.tok_lcurldollar_pat_0e8e4b6) =
  (* tok_lcurldollar_pat_0e8e4b6 *) token env tok

let map_use_type (env : env) (x : CST.use_type) =
  (match x with
  | `Name tok -> (* "namespace" *) token env tok
  | `Func tok -> (* "function" *) token env tok
  | `Type tok -> (* "type" *) token env tok
  | `Const tok -> (* "const" *) token env tok
  )

let map_string_ (env : env) (tok : CST.string_) =
  (* string *) token env tok

let map_float_ (env : env) (tok : CST.float_) =
  (* float *) token env tok

let map_integer (env : env) (tok : CST.integer) =
  (* integer *) token env tok

let map_heredoc_start_newline (env : env) (tok : CST.heredoc_start_newline) =
  (* heredoc_start_newline *) token env tok

let map_xhp_comment (env : env) (tok : CST.xhp_comment) =
  (* xhp_comment *) token env tok

let map_scope_identifier (env : env) (x : CST.scope_identifier) =
  (match x with
  | `Self tok -> (* "self" *) token env tok
  | `Parent tok -> (* "parent" *) token env tok
  | `Static tok -> (* "static" *) token env tok
  )

let map_visibility_modifier (env : env) (x : CST.visibility_modifier) =
  (match x with
  | `Public tok -> (* "public" *) token env tok
  | `Prot tok -> (* "protected" *) token env tok
  | `Priv tok -> (* "private" *) token env tok
  )

let map_collection_type (env : env) (x : CST.collection_type) =
  (match x with
  | `Array tok -> (* "array" *) token env tok
  | `Varray tok -> (* "varray" *) token env tok
  | `Darray tok -> (* "darray" *) token env tok
  | `Vec tok -> (* "vec" *) token env tok
  | `Dict tok -> (* "dict" *) token env tok
  | `Keyset tok -> (* "keyset" *) token env tok
  )

let map_xhp_identifier (env : env) (tok : CST.xhp_identifier) =
  (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok

let map_type_modifier (env : env) (x : CST.type_modifier) =
  (match x with
  | `AT tok -> (* "@" *) token env tok
  | `QMARK tok -> (* "?" *) token env tok
  | `TILDE tok -> (* "~" *) token env tok
  )

let map_null (env : env) (x : CST.null) =
  (match x with
  | `Null_37a6259 tok -> (* "null" *) token env tok
  | `Null_bbb93ef tok -> (* "Null" *) token env tok
  | `NULL tok -> (* "NULL" *) token env tok
  )

let map_anon_choice_QMARKDASHGT_ce9cc19 (env : env) (x : CST.anon_choice_QMARKDASHGT_ce9cc19) =
  (match x with
  | `QMARKDASHGT tok -> (* "?->" *) token env tok
  | `DASHGT tok -> (* "->" *) token env tok
  )

let map_heredoc_start (env : env) (tok : CST.heredoc_start) =
  (* heredoc_start *) token env tok

let map_xhp_category_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_category_declaration) =
  let v1 = (* "category" *) token env v1 in
  let v2 =
    (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v2
  in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 =
        (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v2
      in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

let map_empty_statement (env : env) (x : CST.empty_statement) =
  (match x with
  | `SEMI tok -> (* ";" *) token env tok
  | `Ellips tok -> (* "..." *) token env tok
  )

let map_semgrep_extended_identifier (env : env) (x : CST.semgrep_extended_identifier) =
  (match x with
  | `Semg_id tok ->
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
  | `Id tok ->
      (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
  )

let map_qualified_identifier (env : env) (x : CST.qualified_identifier) =
  (match x with
  | `Choice_opt_id_rep1_back_id x ->
      (match x with
      | `Opt_id_rep1_back_id (v1, v2) ->
          let v1 =
            (match v1 with
            | Some tok ->
                (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
            | None -> todo env ())
          in
          let v2 =
            List.map (fun (v1, v2) ->
              let v1 = (* "\\" *) token env v1 in
              let v2 =
                (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v2
              in
              todo env (v1, v2)
            ) v2
          in
          todo env (v1, v2)
      | `Id tok ->
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
      )
  | `Semg_id tok ->
      (* pattern \$[A-Z_][A-Z_0-9]* *) token env tok
  )

let map_prefixed_string (env : env) ((v1, v2) : CST.prefixed_string) =
  let v1 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v1
  in
  let v2 = (* string *) token env v2 in
  todo env (v1, v2)

let map_anon_choice_str_d42aa42 (env : env) (x : CST.anon_choice_str_d42aa42) =
  (match x with
  | `Str tok -> (* string *) token env tok
  | `Int tok -> (* integer *) token env tok
  )

let map_trait_alias_clause (env : env) ((v1, v2, v3) : CST.trait_alias_clause) =
  let v1 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v1
  in
  let v2 = (* "as" *) token env v2 in
  let v3 =
    (match v3 with
    | `Visi_modi_opt_id (v1, v2) ->
        let v1 = map_visibility_modifier env v1 in
        let v2 =
          (match v2 with
          | Some tok ->
              (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
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
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v2
        in
        todo env (v1, v2)
    )
  in
  todo env (v1, v2, v3)

let map_class_modifier (env : env) (x : CST.class_modifier) =
  (match x with
  | `Abst_modi tok -> (* "abstract" *) token env tok
  | `Final_modi tok -> (* "final" *) token env tok
  )

let map_xhp_identifier_ (env : env) (x : CST.xhp_identifier_) =
  (match x with
  | `Xhp_id tok ->
      (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_class_id tok ->
      (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  )

let rec map_xhp_attribute_expression (env : env) (x : CST.xhp_attribute_expression) =
  (match x with
  | `Xhp_id tok ->
      (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_class_id tok ->
      (* pattern :[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_cate_id tok ->
      (* pattern %[a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
  | `Xhp_bin_exp (v1, v2, v3) ->
      let v1 = map_xhp_attribute_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_xhp_attribute_expression env v3 in
      todo env (v1, v2, v3)
  | `Xhp_post_un_exp (v1, v2) ->
      let v1 = map_xhp_attribute_expression env v1 in
      let v2 =
        (match v2 with
        | `PLUS tok -> (* "+" *) token env tok
        | `STAR tok -> (* "*" *) token env tok
        | `QMARK tok -> (* "?" *) token env tok
        )
      in
      todo env (v1, v2)
  | `Xhp_paren_exp (v1, v2, v3, v4) ->
      let v1 = (* "(" *) token env v1 in
      let v2 = map_xhp_attribute_expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_xhp_attribute_expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = (* ")" *) token env v4 in
      todo env (v1, v2, v3, v4)
  )

let map_primitive_type (env : env) (x : CST.primitive_type) =
  (match x with
  | `Bool tok -> (* "bool" *) token env tok
  | `Float tok -> (* "float" *) token env tok
  | `Int tok -> (* "int" *) token env tok
  | `Str tok -> (* "string" *) token env tok
  | `Arra tok -> (* "arraykey" *) token env tok
  | `Void tok -> (* "void" *) token env tok
  | `Nonn tok -> (* "nonnull" *) token env tok
  | `Null x -> map_null env x
  | `Mixed tok -> (* "mixed" *) token env tok
  | `Dyna tok -> (* "dynamic" *) token env tok
  | `Nore tok -> (* "noreturn" *) token env tok
  )

let map_literal (env : env) (x : CST.literal) =
  (match x with
  | `Str tok -> (* string *) token env tok
  | `Int tok -> (* integer *) token env tok
  | `Float tok -> (* float *) token env tok
  | `True x -> map_true_ env x
  | `False x -> map_false_ env x
  | `Null x -> map_null env x
  )

let map_member_modifier (env : env) (x : CST.member_modifier) =
  (match x with
  | `Visi_modi x -> map_visibility_modifier env x
  | `Static_modi tok -> (* "static" *) token env tok
  | `Abst_modi tok -> (* "abstract" *) token env tok
  | `Final_modi tok -> (* "final" *) token env tok
  )

let rec map_type_constant_ (env : env) ((v1, v2, v3) : CST.type_constant_) =
  let v1 =
    (match v1 with
    | `Qual_id x -> map_qualified_identifier env x
    | `Type_cst_ x -> map_type_constant_ env x
    )
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  todo env (v1, v2, v3)

let map_namespace_identifier (env : env) (x : CST.namespace_identifier) =
  (match x with
  | `Qual_id_opt_back (v1, v2) ->
      let v1 = map_qualified_identifier env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "\\" *) token env tok
        | None -> todo env ())
      in
      todo env (v1, v2)
  | `Back tok -> (* "\\" *) token env tok
  )

let map_trait_select_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.trait_select_clause) =
  let v1 = map_qualified_identifier env v1 in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  let v4 = (* "insteadof" *) token env v4 in
  let v5 = map_qualified_identifier env v5 in
  let v6 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_qualified_identifier env v2 in
      todo env (v1, v2)
    ) v6
  in
  todo env (v1, v2, v3, v4, v5, v6)

let map_xhp_enum_type (env : env) ((v1, v2, v3, v4, v5, v6) : CST.xhp_enum_type) =
  let v1 = (* "enum" *) token env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 = map_anon_choice_str_d42aa42 env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_str_d42aa42 env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v6 = (* "}" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let map_xhp_close (env : env) ((v1, v2, v3) : CST.xhp_close) =
  let v1 = (* "</" *) token env v1 in
  let v2 = map_xhp_identifier_ env v2 in
  let v3 = (* ">" *) token env v3 in
  todo env (v1, v2, v3)

let map_xhp_children_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_children_declaration) =
  let v1 = (* "children" *) token env v1 in
  let v2 = map_xhp_attribute_expression env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_xhp_attribute_expression env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

let map_keyword (env : env) (x : CST.keyword) =
  (match x with
  | `Type tok -> (* "type" *) token env tok
  | `Newt tok -> (* "newtype" *) token env tok
  | `Shape tok -> (* "shape" *) token env tok
  | `Tupe tok -> (* "tupe" *) token env tok
  | `Clone tok -> (* "clone" *) token env tok
  | `New tok -> (* "new" *) token env tok
  | `Print tok -> (* "print" *) token env tok
  | `Choice_bool x -> map_primitive_type env x
  | `Choice_array x -> map_collection_type env x
  )

let map_anonymous_function_use_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_use_clause) =
  let v1 = (* "use" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = (* variable *) token env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = (* variable *) token env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v6 = (* ")" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

let map_scoped_identifier (env : env) ((v1, v2, v3) : CST.scoped_identifier) =
  let v1 =
    (match v1 with
    | `Qual_id x -> map_qualified_identifier env x
    | `Var tok -> (* variable *) token env tok
    | `Scope_id x -> map_scope_identifier env x
    | `Choice_xhp_id x -> map_xhp_identifier_ env x
    | `Pipe_var tok -> (* "$$" *) token env tok
    )
  in
  let v2 = (* "::" *) token env v2 in
  let v3 =
    (match v3 with
    | `Id tok ->
        (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env tok
    | `Var tok -> (* variable *) token env tok
    )
  in
  todo env (v1, v2, v3)

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
        let v1 = (* "as" *) token env v1 in
        let v2 =
          (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v2
        in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

let map_anon_choice_semg_exte_id_8bbc8de (env : env) (x : CST.anon_choice_semg_exte_id_8bbc8de) =
  (match x with
  | `Semg_exte_id x -> map_semgrep_extended_identifier env x
  | `Choice_type x -> map_keyword env x
  )

let rec map_anon_choice_comp_stmt_c6c6bb4 (env : env) (x : CST.anon_choice_comp_stmt_c6c6bb4) =
  (match x with
  | `Comp_stmt x -> map_compound_statement env x
  | `SEMI tok -> (* ";" *) token env tok
  )

and map_anon_choice_exp_1701d0a (env : env) (x : CST.anon_choice_exp_1701d0a) =
  (match x with
  | `Exp x -> map_expression env x
  | `Elem_init (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb (env : env) ((v1, v2, v3) : CST.anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb) =
  let v1 = map_anon_choice_exp_1701d0a env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_anon_choice_exp_1701d0a env v2 in
      todo env (v1, v2)
    ) v2
  in
  let v3 =
    (match v3 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  todo env (v1, v2, v3)

and map_anon_choice_field_spec_0e0e023 (env : env) (x : CST.anon_choice_field_spec_0e0e023) =
  (match x with
  | `Field_spec (v1, v2, v3, v4) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "?" *) token env tok
        | None -> todo env ())
      in
      let v2 = map_expression env v2 in
      let v3 = (* "=>" *) token env v3 in
      let v4 = map_type_ env v4 in
      todo env (v1, v2, v3, v4)
  | `DOTDOTDOT tok -> (* "..." *) token env tok
  )

and map_anon_exp_rep_COMMA_exp_0bb260c (env : env) ((v1, v2) : CST.anon_exp_rep_COMMA_exp_0bb260c) =
  let v1 = map_expression env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
    ) v2
  in
  todo env (v1, v2)

and map_anonymous_function_expression (env : env) ((v1, v2, v3, v4, v5, v6) : CST.anonymous_function_expression) =
  let v1 =
    (match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = map_parameters env v3 in
  let v4 =
    (match v4 with
    | Some (v1, v2) ->
        let v1 = (* ":" *) token env v1 in
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

and map_argument (env : env) ((v1, v2) : CST.argument) =
  let v1 =
    (match v1 with
    | Some x ->
        (match x with
        | `Inout_modi tok -> (* "inout" *) token env tok
        | `Vari_modi tok -> (* "..." *) token env tok
        )
    | None -> todo env ())
  in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_arguments (env : env) ((v1, v2, v3) : CST.arguments) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_argument env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_argument env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_array_ (env : env) ((v1, v2, v3, v4, v5) : CST.array_) =
  let v1 = map_collection_type env v1 in
  let v2 =
    (match v2 with
    | Some x -> map_type_arguments env x
    | None -> todo env ())
  in
  let v3 = (* "[" *) token env v3 in
  let v4 =
    (match v4 with
    | Some x ->
        map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
    | None -> todo env ())
  in
  let v5 = (* "]" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_as_expression (env : env) ((v1, v2, v3) : CST.as_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `As tok -> (* as *) token env tok
    | `QMARKas tok -> (* "?as" *) token env tok
    )
  in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_attribute_modifier (env : env) ((v1, v2, v3, v4, v5, v6) : CST.attribute_modifier) =
  let v1 = (* "<<" *) token env v1 in
  let v2 = map_qualified_identifier env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_arguments env x
    | None -> todo env ())
  in
  let v4 =
    List.map (fun (v1, v2, v3) ->
      let v1 = (* "," *) token env v1 in
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
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v6 = (* ">>" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_awaitable_expression (env : env) ((v1, v2) : CST.awaitable_expression) =
  let v1 = (* "async" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  todo env (v1, v2)

and map_binary_expression (env : env) (x : CST.binary_expression) =
  (match x with
  | `Exp_BARGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKQMARK_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "??" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BARBAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "||" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPAMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HAT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMP_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "===" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BANGEQEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "!==" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTEQGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<=>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUS_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DOT_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "." *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASH_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERC_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAR_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKCOLON_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "?:" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_EQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_QMARKQMARKEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "??=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DOTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ".=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_BAREQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "|=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_HATEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "^=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_AMPEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "&=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_LTLTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "<<=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_GTGTEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* ">>=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PLUSEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "+=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_DASHEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "-=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STAREQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "*=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_SLASHEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "/=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_PERCEQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "%=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  | `Exp_STARSTAREQ_exp (v1, v2, v3) ->
      let v1 = map_expression env v1 in
      let v2 = (* "**=" *) token env v2 in
      let v3 = map_expression env v3 in
      todo env (v1, v2, v3)
  )

and map_braced_expression (env : env) ((v1, v2, v3) : CST.braced_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

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

and map_cast_expression (env : env) ((v1, v2, v3, v4) : CST.cast_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | `Array tok -> (* "array" *) token env tok
    | `Int tok -> (* "int" *) token env tok
    | `Float tok -> (* "float" *) token env tok
    | `Str tok -> (* "string" *) token env tok
    | `Bool tok -> (* "bool" *) token env tok
    )
  in
  let v3 = (* ")" *) token env v3 in
  let v4 = map_expression env v4 in
  todo env (v1, v2, v3, v4)

and map_catch_clause (env : env) ((v1, v2, v3, v4, v5, v6) : CST.catch_clause) =
  let v1 = (* "catch" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 = (* variable *) token env v4 in
  let v5 = (* ")" *) token env v5 in
  let v6 = map_compound_statement env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_class_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6) : CST.class_const_declaration) =
  let v1 = List.map (map_member_modifier env) v1 in
  let v2 = (* "const" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_type_ env x
    | None -> todo env ())
  in
  let v4 = map_class_const_declarator env v4 in
  let v5 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_class_const_declarator env v2 in
      todo env (v1, v2)
    ) v5
  in
  let v6 = (* ";" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_class_const_declarator (env : env) ((v1, v2) : CST.class_const_declarator) =
  let v1 = map_anon_choice_semg_exte_id_8bbc8de env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_collection (env : env) ((v1, v2, v3, v4) : CST.collection) =
  let v1 = map_qualified_identifier env v1 in
  let v2 = (* "{" *) token env v2 in
  let v3 =
    (match v3 with
    | Some x ->
        map_anon_choice_exp_rep_COMMA_choice_exp_opt_COMMA_e4364bb env x
    | None -> todo env ())
  in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_compound_statement (env : env) ((v1, v2, v3) : CST.compound_statement) =
  let v1 = (* "{" *) token env v1 in
  let v2 = List.map (map_statement env) v2 in
  let v3 = (* "}" *) token env v3 in
  todo env (v1, v2, v3)

and map_const_declarator (env : env) ((v1, v2, v3) : CST.const_declarator) =
  let v1 = map_anon_choice_semg_exte_id_8bbc8de env v1 in
  let v2 = (* "=" *) token env v2 in
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
  | `Class_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) ->
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
      let v4 =
        (match v4 with
        | Some tok -> (* "xhp" *) token env tok
        | None -> todo env ())
      in
      let v5 = (* "class" *) token env v5 in
      let v6 =
        (match v6 with
        | `Semg_exte_id x -> map_semgrep_extended_identifier env x
        | `Choice_xhp_id x -> map_xhp_identifier_ env x
        )
      in
      let v7 =
        (match v7 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v8 =
        (match v8 with
        | Some x -> map_extends_clause env x
        | None -> todo env ())
      in
      let v9 =
        (match v9 with
        | Some x -> map_implements_clause env x
        | None -> todo env ())
      in
      let v10 =
        (match v10 with
        | Some x -> map_where_clause env x
        | None -> todo env ())
      in
      let v11 = map_member_declarations env v11 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)
  | `Inte_decl (v1, v2, v3, v4, v5, v6, v7) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "interface" *) token env v2 in
      let v3 = map_semgrep_extended_identifier env v3 in
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
      let v2 = (* "trait" *) token env v2 in
      let v3 = map_semgrep_extended_identifier env v3 in
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
        | `Type tok -> (* "type" *) token env tok
        | `Newt tok -> (* "newtype" *) token env tok
        )
      in
      let v3 = map_semgrep_extended_identifier env v3 in
      let v4 =
        (match v4 with
        | Some x -> map_type_parameters env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some (v1, v2) ->
            let v1 = (* "as" *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v6 = (* "=" *) token env v6 in
      let v7 = map_type_ env v7 in
      let v8 = (* ";" *) token env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Enum_decl (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 =
        (match v1 with
        | Some x -> map_attribute_modifier env x
        | None -> todo env ())
      in
      let v2 = (* "enum" *) token env v2 in
      let v3 = map_semgrep_extended_identifier env v3 in
      let v4 = (* ":" *) token env v4 in
      let v5 = map_type_ env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = (* "as" *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 = (* "{" *) token env v7 in
      let v8 = List.map (map_enumerator env) v8 in
      let v9 = (* "}" *) token env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Name_decl (v1, v2) ->
      let v1 = (* "namespace" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x ->
            (match x with
            | `Qual_id_SEMI (v1, v2) ->
                let v1 = map_qualified_identifier env v1 in
                let v2 = (* ";" *) token env v2 in
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
      let v1 = (* "const" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_type_ env x
        | None -> todo env ())
      in
      let v3 = map_const_declarator env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_const_declarator env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 = (* ";" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  )

and map_embedded_brace_expression (env : env) ((v1, v2) : CST.embedded_brace_expression) =
  let v1 = map_embedded_brace_expression_ env v1 in
  let v2 = (* "}" *) token env v2 in
  todo env (v1, v2)

and map_embedded_brace_expression_ (env : env) (x : CST.embedded_brace_expression_) =
  (match x with
  | `Tok_lcur_pat_0e8e4b6 tok ->
      (* tok_lcurldollar_pat_0e8e4b6 *) token env tok
  | `Embe_brace_call_exp (v1, v2) ->
      let v1 = map_embedded_brace_expression_ env v1 in
      let v2 = map_arguments env v2 in
      todo env (v1, v2)
  | `Embe_brace_subs_exp (v1, v2, v3, v4) ->
      let v1 = map_embedded_brace_expression_ env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Embe_brace_sele_exp (v1, v2, v3) ->
      let v1 = map_embedded_brace_expression_ env v1 in
      let v2 = map_anon_choice_QMARKDASHGT_ce9cc19 env v2 in
      let v3 = map_variablish env v3 in
      todo env (v1, v2, v3)
  )

and map_enumerator (env : env) ((v1, v2, v3, v4) : CST.enumerator) =
  let v1 = map_semgrep_extended_identifier env v1 in
  let v2 = (* "=" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_expression (env : env) (x : CST.expression) =
  (match x with
  | `Choice_here x ->
      (match x with
      | `Here x -> map_heredoc env x
      | `Array x -> map_array_ env x
      | `Tuple x -> map_tuple env x
      | `Shape x -> map_shape env x
      | `Coll x -> map_collection env x
      | `Choice_str x -> map_literal env x
      | `Choice_var x -> map_variablish env x
      | `Pref_str x -> map_prefixed_string env x
      | `Paren_exp x -> map_parenthesized_expression env x
      | `Bin_exp x -> map_binary_expression env x
      | `Prefix_un_exp x -> map_prefix_unary_expression env x
      | `Post_un_exp x -> map_postfix_unary_expression env x
      | `Is_exp x -> map_is_expression env x
      | `As_exp x -> map_as_expression env x
      | `Awai_exp x -> map_awaitable_expression env x
      | `Yield_exp x -> map_yield_expression env x
      | `Cast_exp x -> map_cast_expression env x
      | `Tern_exp x -> map_ternary_expression env x
      | `Lambda_exp x -> map_lambda_expression env x
      | `Call_exp x -> map_call_expression env x
      | `Sele_exp x -> map_selection_expression env x
      | `New_exp x -> map_new_expression env x
      | `Incl_exp x -> map_include_expression env x
      | `Requ_exp x -> map_require_expression env x
      | `Anon_func_exp x ->
          map_anonymous_function_expression env x
      | `Xhp_exp x -> map_xhp_expression env x
      )
  | `Ellips tok -> (* "..." *) token env tok
  | `Deep_ellips (v1, v2, v3) ->
      let v1 = (* "<..." *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* "...>" *) token env v3 in
      todo env (v1, v2, v3)
  )

and map_expression_statement (env : env) ((v1, v2) : CST.expression_statement) =
  let v1 = map_expression env v1 in
  let v2 = (* ";" *) token env v2 in
  todo env (v1, v2)

and map_extends_clause (env : env) ((v1, v2, v3) : CST.extends_clause) =
  let v1 = (* "extends" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_field_initializer (env : env) ((v1, v2, v3) : CST.field_initializer) =
  let v1 =
    (match v1 with
    | `Str tok -> (* string *) token env tok
    | `Scoped_id x -> map_scoped_identifier env x
    )
  in
  let v2 = (* "=>" *) token env v2 in
  let v3 = map_expression env v3 in
  todo env (v1, v2, v3)

and map_finally_clause (env : env) ((v1, v2) : CST.finally_clause) =
  let v1 = (* "finally" *) token env v1 in
  let v2 = map_compound_statement env v2 in
  todo env (v1, v2)

and map_function_declaration_header (env : env) ((v1, v2, v3, v4, v5, v6, v7) : CST.function_declaration_header) =
  let v1 =
    (match v1 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v2 = (* "function" *) token env v2 in
  let v3 = map_semgrep_extended_identifier env v3 in
  let v4 =
    (match v4 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v5 = map_parameters env v5 in
  let v6 =
    (match v6 with
    | Some (v1, v2, v3) ->
        let v1 = (* ":" *) token env v1 in
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

and map_heredoc (env : env) ((v1, v2, v3, v4, v5, v6) : CST.heredoc) =
  let v1 = (* "<<<" *) token env v1 in
  let v2 = (* heredoc_start *) token env v2 in
  let v3 =
    (match v3 with
    | Some tok -> (* heredoc_start_newline *) token env tok
    | None -> todo env ())
  in
  let v4 =
    List.map (fun x ->
      (match x with
      | `Here_body tok -> (* heredoc_body *) token env tok
      | `Var tok -> (* variable *) token env tok
      | `Embe_brace_exp x -> map_embedded_brace_expression env x
      )
    ) v4
  in
  let v5 =
    (match v5 with
    | Some tok -> (* heredoc_end_newline *) token env tok
    | None -> todo env ())
  in
  let v6 = (* heredoc_end *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_implements_clause (env : env) ((v1, v2, v3) : CST.implements_clause) =
  let v1 = (* "implements" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  todo env (v1, v2, v3)

and map_include_expression (env : env) ((v1, v2) : CST.include_expression) =
  let v1 =
    (match v1 with
    | `Incl tok -> (* "include" *) token env tok
    | `Incl_once tok -> (* "include_once" *) token env tok
    )
  in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_is_expression (env : env) ((v1, v2, v3) : CST.is_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "is" *) token env v2 in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_lambda_expression (env : env) ((v1, v2, v3, v4, v5) : CST.lambda_expression) =
  let v1 =
    (match v1 with
    | Some x -> map_attribute_modifier env x
    | None -> todo env ())
  in
  let v2 =
    (match v2 with
    | Some tok -> (* "async" *) token env tok
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | `Single_param_params tok -> (* variable *) token env tok
    | `Params_opt_COLON_choice_type_spec (v1, v2) ->
        let v1 = map_parameters env v1 in
        let v2 =
          (match v2 with
          | Some (v1, v2) ->
              let v1 = (* ":" *) token env v1 in
              let v2 = map_type_ env v2 in
              todo env (v1, v2)
          | None -> todo env ())
        in
        todo env (v1, v2)
    )
  in
  let v4 = (* "==>" *) token env v4 in
  let v5 =
    (match v5 with
    | `Exp x -> map_expression env x
    | `Comp_stmt x -> map_compound_statement env x
    )
  in
  todo env (v1, v2, v3, v4, v5)

and map_member_declarations (env : env) ((v1, v2, v3) : CST.member_declarations) =
  let v1 = (* "{" *) token env v1 in
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
      | `Ellips tok -> (* "..." *) token env tok
      )
    ) v2
  in
  let v3 = (* "}" *) token env v3 in
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

and map_new_expression (env : env) ((v1, v2, v3, v4) : CST.new_expression) =
  let v1 = (* "new" *) token env v1 in
  let v2 = map_variablish env v2 in
  let v3 =
    (match v3 with
    | Some x -> map_type_arguments env x
    | None -> todo env ())
  in
  let v4 = map_arguments env v4 in
  todo env (v1, v2, v3, v4)

and map_parameter (env : env) (x : CST.parameter) =
  (match x with
  | `Opt_attr_modi_opt_visi_modi_opt_inout_modi_opt_choice_type_spec_opt_vari_modi_var_opt_EQ_exp (v1, v2, v3, v4, v5, v6, v7) ->
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
        | Some tok -> (* "inout" *) token env tok
        | None -> todo env ())
      in
      let v4 =
        (match v4 with
        | Some x -> map_type_ env x
        | None -> todo env ())
      in
      let v5 =
        (match v5 with
        | Some tok -> (* "..." *) token env tok
        | None -> todo env ())
      in
      let v6 = (* variable *) token env v6 in
      let v7 =
        (match v7 with
        | Some (v1, v2) ->
            let v1 = (* "=" *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5, v6, v7)
  | `Ellips tok -> (* "..." *) token env tok
  )

and map_parameters (env : env) ((v1, v2, v3) : CST.parameters) =
  let v1 = (* "(" *) token env v1 in
  let v2 =
    (match v2 with
    | Some x ->
        (match x with
        | `Vari_modi tok -> (* "..." *) token env tok
        | `Param_rep_COMMA_param_opt_COMMA (v1, v2, v3) ->
            let v1 = map_parameter env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_parameter env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> (* "," *) token env tok
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        )
    | None -> todo env ())
  in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_parenthesized_expression (env : env) ((v1, v2, v3) : CST.parenthesized_expression) =
  let v1 = (* "(" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ")" *) token env v3 in
  todo env (v1, v2, v3)

and map_postfix_unary_expression (env : env) ((v1, v2) : CST.postfix_unary_expression) =
  let v1 = map_expression env v1 in
  let v2 =
    (match v2 with
    | `PLUSPLUS tok -> (* "++" *) token env tok
    | `DASHDASH tok -> (* "--" *) token env tok
    )
  in
  todo env (v1, v2)

and map_prefix_unary_expression (env : env) (x : CST.prefix_unary_expression) =
  (match x with
  | `BANG_exp (v1, v2) ->
      let v1 = (* "!" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `TILDE_exp (v1, v2) ->
      let v1 = (* "~" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `DASH_exp (v1, v2) ->
      let v1 = (* "-" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `PLUS_exp (v1, v2) ->
      let v1 = (* "+" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `PLUSPLUS_exp (v1, v2) ->
      let v1 = (* "++" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `DASHDASH_exp (v1, v2) ->
      let v1 = (* "--" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Print_exp (v1, v2) ->
      let v1 = (* "print" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Clone_exp (v1, v2) ->
      let v1 = (* "clone" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `Await_exp (v1, v2) ->
      let v1 = (* "await" *) token env v1 in
      let v2 = map_expression env v2 in
      todo env (v1, v2)
  | `AT_exp (v1, v2) ->
      let v1 = (* "@" *) token env v1 in
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
      let v1 = (* "," *) token env v1 in
      let v2 = map_property_declarator env v2 in
      todo env (v1, v2)
    ) v5
  in
  let v6 = (* ";" *) token env v6 in
  todo env (v1, v2, v3, v4, v5, v6)

and map_property_declarator (env : env) ((v1, v2) : CST.property_declarator) =
  let v1 = (* variable *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  todo env (v1, v2)

and map_require_expression (env : env) ((v1, v2) : CST.require_expression) =
  let v1 =
    (match v1 with
    | `Requ tok -> (* "require" *) token env tok
    | `Requ_once tok -> (* "require_once" *) token env tok
    )
  in
  let v2 = map_expression env v2 in
  todo env (v1, v2)

and map_require_extends_clause (env : env) ((v1, v2, v3, v4, v5) : CST.require_extends_clause) =
  let v1 = (* "require" *) token env v1 in
  let v2 = (* "extends" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 = (* ";" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_require_implements_clause (env : env) ((v1, v2, v3, v4, v5) : CST.require_implements_clause) =
  let v1 = (* "require" *) token env v1 in
  let v2 = (* "implements" *) token env v2 in
  let v3 = map_type_ env v3 in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  let v5 = (* ";" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_selection_expression (env : env) ((v1, v2, v3) : CST.selection_expression) =
  let v1 =
    (match v1 with
    | `Choice_var x -> map_variablish env x
    | `As_exp x -> map_as_expression env x
    )
  in
  let v2 = map_anon_choice_QMARKDASHGT_ce9cc19 env v2 in
  let v3 =
    (match v3 with
    | `Choice_var x -> map_variablish env x
    | `Braced_exp x -> map_braced_expression env x
    | `Choice_type x -> map_keyword env x
    )
  in
  todo env (v1, v2, v3)

and map_shape (env : env) ((v1, v2, v3, v4) : CST.shape) =
  let v1 = (* "shape" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) ->
        let v1 = map_field_initializer env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_field_initializer env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v4 = (* ")" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_statement (env : env) (x : CST.statement) =
  (match x with
  | `Choice_func_decl x -> map_declaration env x
  | `Comp_stmt x -> map_compound_statement env x
  | `Empty_stmt x -> map_empty_statement env x
  | `Exp_stmt x -> map_expression_statement env x
  | `Ret_stmt (v1, v2, v3) ->
      let v1 = (* "return" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `Brk_stmt (v1, v2, v3) ->
      let v1 = (* "break" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `Cont_stmt (v1, v2, v3) ->
      let v1 = (* "continue" *) token env v1 in
      let v2 =
        (match v2 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `Throw_stmt (v1, v2, v3) ->
      let v1 = (* "throw" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `Echo_stmt (v1, v2, v3, v4) ->
      let v1 = (* "echo" *) token env v1 in
      let v2 = map_expression env v2 in
      let v3 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_expression env v2 in
          todo env (v1, v2)
        ) v3
      in
      let v4 = (* ";" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Unset_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "unset" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some (v1, v2) ->
            let v1 = map_variablish env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_variablish env v2 in
                todo env (v1, v2)
              ) v2
            in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v4 = (* ")" *) token env v4 in
      let v5 = (* ";" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Use_stmt (v1, v2, v3) ->
      let v1 = (* "use" *) token env v1 in
      let v2 =
        (match v2 with
        | `Use_clause_rep_COMMA_use_clause_opt_COMMA (v1, v2, v3) ->
            let v1 = map_use_clause env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_use_clause env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> (* "," *) token env tok
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
            let v3 = (* "{" *) token env v3 in
            let v4 = map_use_clause env v4 in
            let v5 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_use_clause env v2 in
                todo env (v1, v2)
              ) v5
            in
            let v6 =
              (match v6 with
              | Some tok -> (* "," *) token env tok
              | None -> todo env ())
            in
            let v7 = (* "}" *) token env v7 in
            todo env (v1, v2, v3, v4, v5, v6, v7)
        )
      in
      let v3 = (* ";" *) token env v3 in
      todo env (v1, v2, v3)
  | `If_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "if" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      let v4 =
        List.map (fun (v1, v2, v3) ->
          let v1 =
            (match v1 with
            | `Elseif tok -> (* "elseif" *) token env tok
            | `Else_if (v1, v2) ->
                let v1 = (* "else" *) token env v1 in
                let v2 = (* "if" *) token env v2 in
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
            let v1 = (* "else" *) token env v1 in
            let v2 = map_statement env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      todo env (v1, v2, v3, v4, v5)
  | `While_stmt (v1, v2, v3) ->
      let v1 = (* "while" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = map_statement env v3 in
      todo env (v1, v2, v3)
  | `Do_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "do" *) token env v1 in
      let v2 = map_statement env v2 in
      let v3 = (* "while" *) token env v3 in
      let v4 = map_parenthesized_expression env v4 in
      let v5 = (* ";" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `For_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* "for" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v4 = (* ";" *) token env v4 in
      let v5 =
        (match v5 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v6 = (* ";" *) token env v6 in
      let v7 =
        (match v7 with
        | Some x -> map_anon_exp_rep_COMMA_exp_0bb260c env x
        | None -> todo env ())
      in
      let v8 = (* ")" *) token env v8 in
      let v9 = map_statement env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Switch_stmt (v1, v2, v3, v4, v5) ->
      let v1 = (* "switch" *) token env v1 in
      let v2 = map_parenthesized_expression env v2 in
      let v3 = (* "{" *) token env v3 in
      let v4 =
        List.map (fun x ->
          (match x with
          | `Switch_case x -> map_switch_case env x
          | `Switch_defa x -> map_switch_default env x
          )
        ) v4
      in
      let v5 = (* "}" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Fore_stmt (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
      let v1 = (* "foreach" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_expression env v3 in
      let v4 =
        (match v4 with
        | Some tok -> (* "await" *) token env tok
        | None -> todo env ())
      in
      let v5 = (* as *) token env v5 in
      let v6 =
        (match v6 with
        | Some (v1, v2) ->
            let v1 = map_variablish env v1 in
            let v2 = (* "=>" *) token env v2 in
            todo env (v1, v2)
        | None -> todo env ())
      in
      let v7 = map_variablish env v7 in
      let v8 = (* ")" *) token env v8 in
      let v9 = map_statement env v9 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8, v9)
  | `Try_stmt (v1, v2, v3, v4) ->
      let v1 = (* "try" *) token env v1 in
      let v2 = map_compound_statement env v2 in
      let v3 = List.map (map_catch_clause env) v3 in
      let v4 =
        (match v4 with
        | `Catch_clause x -> map_catch_clause env x
        | `Fina_clause x -> map_finally_clause env x
        )
      in
      todo env (v1, v2, v3, v4)
  | `Conc_stmt (v1, v2) ->
      let v1 = (* "concurrent" *) token env v1 in
      let v2 = map_compound_statement env v2 in
      todo env (v1, v2)
  | `Using_stmt (v1, v2, v3) ->
      let v1 =
        (match v1 with
        | Some tok -> (* "await" *) token env tok
        | None -> todo env ())
      in
      let v2 = (* "using" *) token env v2 in
      let v3 =
        (match v3 with
        | `Exp_stmt x -> map_expression_statement env x
        | `LPAR_exp_rep_COMMA_exp_RPAR_choice_comp_stmt (v1, v2, v3, v4, v5) ->
            let v1 = (* "(" *) token env v1 in
            let v2 = map_expression env v2 in
            let v3 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_expression env v2 in
                todo env (v1, v2)
              ) v3
            in
            let v4 = (* ")" *) token env v4 in
            let v5 = map_anon_choice_comp_stmt_c6c6bb4 env v5 in
            todo env (v1, v2, v3, v4, v5)
        )
      in
      todo env (v1, v2, v3)
  )

and map_switch_case (env : env) ((v1, v2, v3, v4) : CST.switch_case) =
  let v1 = (* "case" *) token env v1 in
  let v2 = map_expression env v2 in
  let v3 = (* ":" *) token env v3 in
  let v4 = List.map (map_statement env) v4 in
  todo env (v1, v2, v3, v4)

and map_switch_default (env : env) ((v1, v2, v3) : CST.switch_default) =
  let v1 = (* "default" *) token env v1 in
  let v2 = (* ":" *) token env v2 in
  let v3 = List.map (map_statement env) v3 in
  todo env (v1, v2, v3)

and map_ternary_expression (env : env) ((v1, v2, v3, v4, v5) : CST.ternary_expression) =
  let v1 = map_expression env v1 in
  let v2 = (* "?" *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* ":" *) token env v4 in
  let v5 = map_expression env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_trait_use_clause (env : env) ((v1, v2, v3, v4) : CST.trait_use_clause) =
  let v1 = (* "use" *) token env v1 in
  let v2 = map_type_ env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | `LCURL_rep_choice_trait_select_clause_SEMI_RCURL (v1, v2, v3) ->
        let v1 = (* "{" *) token env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 =
              (match v1 with
              | `Trait_select_clause x -> map_trait_select_clause env x
              | `Trait_alias_clause x -> map_trait_alias_clause env x
              )
            in
            let v2 = (* ";" *) token env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 = (* "}" *) token env v3 in
        todo env (v1, v2, v3)
    | `SEMI tok -> (* ";" *) token env tok
    )
  in
  todo env (v1, v2, v3, v4)

and map_tuple (env : env) ((v1, v2, v3, v4) : CST.tuple) =
  let v1 = (* "tuple" *) token env v1 in
  let v2 = (* "(" *) token env v2 in
  let v3 =
    (match v3 with
    | Some (v1, v2, v3) ->
        let v1 = map_expression env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_expression env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v4 = (* ")" *) token env v4 in
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
        | `Choice_xhp_id x -> map_xhp_identifier_ env x
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
      let v2 = (* "shape" *) token env v2 in
      let v3 = (* "(" *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3) ->
            let v1 = map_anon_choice_field_spec_0e0e023 env v1 in
            let v2 =
              List.map (fun (v1, v2) ->
                let v1 = (* "," *) token env v1 in
                let v2 = map_anon_choice_field_spec_0e0e023 env v2 in
                todo env (v1, v2)
              ) v2
            in
            let v3 =
              (match v3 with
              | Some tok -> (* "," *) token env tok
              | None -> todo env ())
            in
            todo env (v1, v2, v3)
        | None -> todo env ())
      in
      let v5 = (* ")" *) token env v5 in
      todo env (v1, v2, v3, v4, v5)
  | `Func_type_spec (v1, v2, v3, v4, v5, v6, v7, v8) ->
      let v1 = List.map (map_type_modifier env) v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = (* pattern function\s*\( *) token env v3 in
      let v4 =
        (match v4 with
        | Some (v1, v2, v3, v4, v5) ->
            let v1 =
              (match v1 with
              | Some tok -> (* "inout" *) token env tok
              | None -> todo env ())
            in
            let v2 = map_type_ env v2 in
            let v3 =
              (match v3 with
              | Some tok -> (* "..." *) token env tok
              | None -> todo env ())
            in
            let v4 =
              List.map (fun (v1, v2, v3, v4) ->
                let v1 = (* "," *) token env v1 in
                let v2 =
                  (match v2 with
                  | Some tok -> (* "inout" *) token env tok
                  | None -> todo env ())
                in
                let v3 = map_type_ env v3 in
                let v4 =
                  (match v4 with
                  | Some tok -> (* "..." *) token env tok
                  | None -> todo env ())
                in
                todo env (v1, v2, v3, v4)
              ) v4
            in
            let v5 =
              (match v5 with
              | Some tok -> (* "," *) token env tok
              | None -> todo env ())
            in
            todo env (v1, v2, v3, v4, v5)
        | None -> todo env ())
      in
      let v5 = (* ")" *) token env v5 in
      let v6 = (* ":" *) token env v6 in
      let v7 = map_type_ env v7 in
      let v8 = (* ")" *) token env v8 in
      todo env (v1, v2, v3, v4, v5, v6, v7, v8)
  | `Tuple_type_spec (v1, v2, v3, v4, v5, v6) ->
      let v1 = List.map (map_type_modifier env) v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 = map_type_ env v3 in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
          let v2 = map_type_ env v2 in
          todo env (v1, v2)
        ) v4
      in
      let v5 =
        (match v5 with
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v6 = (* ")" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  )

and map_type_arguments (env : env) ((v1, v2, v3) : CST.type_arguments) =
  let v1 = (* "<" *) token env v1 in
  let v2 =
    (match v2 with
    | Some (v1, v2, v3) ->
        let v1 = map_type_ env v1 in
        let v2 =
          List.map (fun (v1, v2) ->
            let v1 = (* "," *) token env v1 in
            let v2 = map_type_ env v2 in
            todo env (v1, v2)
          ) v2
        in
        let v3 =
          (match v3 with
          | Some tok -> (* "," *) token env tok
          | None -> todo env ())
        in
        todo env (v1, v2, v3)
    | None -> todo env ())
  in
  let v3 = (* ">" *) token env v3 in
  todo env (v1, v2, v3)

and map_type_const_declaration (env : env) ((v1, v2, v3, v4, v5, v6, v7, v8, v9) : CST.type_const_declaration) =
  let v1 =
    (match v1 with
    | Some x -> map_attribute_modifier env x
    | None -> todo env ())
  in
  let v2 = List.map (map_member_modifier env) v2 in
  let v3 = (* "const" *) token env v3 in
  let v4 = (* "type" *) token env v4 in
  let v5 = map_semgrep_extended_identifier env v5 in
  let v6 =
    (match v6 with
    | Some x -> map_type_parameters env x
    | None -> todo env ())
  in
  let v7 =
    (match v7 with
    | Some (v1, v2) ->
        let v1 = (* "as" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v8 =
    (match v8 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_type_ env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v9 = (* ";" *) token env v9 in
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
        | `PLUS tok -> (* "+" *) token env tok
        | `DASH tok -> (* "-" *) token env tok
        | `Reify tok -> (* "reify" *) token env tok
        )
    | None -> todo env ())
  in
  let v3 =
    (* pattern [a-zA-Z_\x80-\xff][a-zA-Z0-9_\x80-\xff]* *) token env v3
  in
  let v4 =
    List.map (fun (v1, v2) ->
      let v1 =
        (match v1 with
        | `As tok -> (* "as" *) token env tok
        | `Super tok -> (* "super" *) token env tok
        )
      in
      let v2 = map_type_ env v2 in
      todo env (v1, v2)
    ) v4
  in
  todo env (v1, v2, v3, v4)

and map_type_parameters (env : env) ((v1, v2, v3, v4, v5) : CST.type_parameters) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_type_parameter env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_type_parameter env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 =
    (match v4 with
    | Some tok -> (* "," *) token env tok
    | None -> todo env ())
  in
  let v5 = (* ">" *) token env v5 in
  todo env (v1, v2, v3, v4, v5)

and map_variablish (env : env) (x : CST.variablish) =
  (match x with
  | `Var tok -> (* variable *) token env tok
  | `Pipe_var tok -> (* "$$" *) token env tok
  | `List_exp (v1, v2, v3, v4, v5, v6) ->
      let v1 = (* "list" *) token env v1 in
      let v2 = (* "(" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 =
        List.map (fun (v1, v2) ->
          let v1 = (* "," *) token env v1 in
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
        | Some tok -> (* "," *) token env tok
        | None -> todo env ())
      in
      let v6 = (* ")" *) token env v6 in
      todo env (v1, v2, v3, v4, v5, v6)
  | `Subs_exp (v1, v2, v3, v4) ->
      let v1 = map_expression env v1 in
      let v2 = (* "[" *) token env v2 in
      let v3 =
        (match v3 with
        | Some x -> map_expression env x
        | None -> todo env ())
      in
      let v4 = (* "]" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Qual_id x -> map_qualified_identifier env x
  | `Paren_exp x -> map_parenthesized_expression env x
  | `Call_exp x -> map_call_expression env x
  | `Scoped_id x -> map_scoped_identifier env x
  | `Scope_id x -> map_scope_identifier env x
  | `Sele_exp x -> map_selection_expression env x
  | `Choice_xhp_id x -> map_xhp_identifier_ env x
  )

and map_where_clause (env : env) ((v1, v2) : CST.where_clause) =
  let v1 = (* "where" *) token env v1 in
  let v2 =
    List.map (fun (v1, v2) ->
      let v1 = map_where_constraint env v1 in
      let v2 =
        (match v2 with
        | Some tok -> (* "," *) token env tok
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
    | `As tok -> (* "as" *) token env tok
    | `Super tok -> (* "super" *) token env tok
    | `EQ tok -> (* "=" *) token env tok
    )
  in
  let v3 = map_type_ env v3 in
  todo env (v1, v2, v3)

and map_xhp_attribute (env : env) (x : CST.xhp_attribute) =
  (match x with
  | `Xhp_id_EQ_choice_str (v1, v2, v3) ->
      let v1 =
        (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env v1
      in
      let v2 = (* "=" *) token env v2 in
      let v3 =
        (match v3 with
        | `Str tok -> (* string *) token env tok
        | `Braced_exp x -> map_braced_expression env x
        )
      in
      todo env (v1, v2, v3)
  | `Choice_braced_exp x ->
      (match x with
      | `Braced_exp x -> map_braced_expression env x
      | `Xhp_spread_exp x -> map_xhp_spread_expression env x
      )
  )

and map_xhp_attribute_declaration (env : env) ((v1, v2, v3, v4) : CST.xhp_attribute_declaration) =
  let v1 = (* "attribute" *) token env v1 in
  let v2 = map_xhp_class_attribute env v2 in
  let v3 =
    List.map (fun (v1, v2) ->
      let v1 = (* "," *) token env v1 in
      let v2 = map_xhp_class_attribute env v2 in
      todo env (v1, v2)
    ) v3
  in
  let v4 = (* ";" *) token env v4 in
  todo env (v1, v2, v3, v4)

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
        (* pattern [a-zA-Z_][a-zA-Z0-9_]*([-:][a-zA-Z0-9_]+)* *) token env tok
    | None -> todo env ())
  in
  let v3 =
    (match v3 with
    | Some (v1, v2) ->
        let v1 = (* "=" *) token env v1 in
        let v2 = map_expression env v2 in
        todo env (v1, v2)
    | None -> todo env ())
  in
  let v4 =
    (match v4 with
    | Some x ->
        (match x with
        | `ATre tok -> (* "@required" *) token env tok
        | `ATla tok -> (* "@lateinit" *) token env tok
        )
    | None -> todo env ())
  in
  todo env (v1, v2, v3, v4)

and map_xhp_expression (env : env) (x : CST.xhp_expression) =
  (match x with
  | `Xhp_open_close (v1, v2, v3, v4) ->
      let v1 = (* "<" *) token env v1 in
      let v2 = map_xhp_identifier_ env v2 in
      let v3 = List.map (map_xhp_attribute env) v3 in
      let v4 = (* "/>" *) token env v4 in
      todo env (v1, v2, v3, v4)
  | `Xhp_open_rep_choice_xhp_str_xhp_close (v1, v2, v3) ->
      let v1 = map_xhp_open env v1 in
      let v2 =
        List.map (fun x ->
          (match x with
          | `Xhp_str tok -> (* xhp_string *) token env tok
          | `Xhp_comm tok -> (* xhp_comment *) token env tok
          | `Braced_exp x -> map_braced_expression env x
          | `Xhp_exp x -> map_xhp_expression env x
          )
        ) v2
      in
      let v3 = map_xhp_close env v3 in
      todo env (v1, v2, v3)
  )

and map_xhp_open (env : env) ((v1, v2, v3, v4) : CST.xhp_open) =
  let v1 = (* "<" *) token env v1 in
  let v2 = map_xhp_identifier_ env v2 in
  let v3 = List.map (map_xhp_attribute env) v3 in
  let v4 = (* ">" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_xhp_spread_expression (env : env) ((v1, v2, v3, v4) : CST.xhp_spread_expression) =
  let v1 = (* "{" *) token env v1 in
  let v2 = (* "..." *) token env v2 in
  let v3 = map_expression env v3 in
  let v4 = (* "}" *) token env v4 in
  todo env (v1, v2, v3, v4)

and map_yield_expression (env : env) ((v1, v2) : CST.yield_expression) =
  let v1 = (* "yield" *) token env v1 in
  let v2 = map_anon_choice_exp_1701d0a env v2 in
  todo env (v1, v2)

let map_script (env : env) ((v1, v2) : CST.script) =
  let v1 =
    (match v1 with
    | Some tok -> (* pattern <\?[hH][hH] *) token env tok
    | None -> todo env ())
  in
  let v2 = List.map (map_statement env) v2 in
  todo env (v1, v2)
