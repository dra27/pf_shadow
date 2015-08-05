(* File: pf_shadow.ml

   An ocaml filter wich warns on value shadowing
   (when a binding silently remplace an outer binding).

   Copyright (C) 2007-
     bluestorm <bluestorm.dylc@gmail.com>

    This program is free software: you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation, either
    version 2 of the License, or (at your option) any later version,
    with the special exception on linking described in the file
    LICENSE.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Library General Public License (file LICENSE) for more details.

   Compilation :
     ocamlfind ocamlc -syntax camlp4o -package camlp4.lib,camlp4.quotations -c pf_shadow.ml
   
   Use :
     camlp4o pf_shadow.cmo your_file.ml
*)

open Camlp4
open Sig

module Id = struct
  let name = "pf_shadow"
  let version = "0.5"
  let description = "Warns on binding shadowing"
end

module Make (Filters : AstFilters) = struct
  open Filters
  
  module Warn_stderr = struct
    (* Warnings : borrowed from OCamlInitSyntax.ml *)
    let print_warning loc txt = Format.eprintf "<W> %a: %s@." Ast.Loc.print loc txt
  end

  module Warn_file = struct
    let output = ref None
    let print_warning loc id shadow =
      let output = match !output with
      | Some out -> out
      | None ->
          let file = Ast.Loc.file_name loc in
          let out = open_out (file ^ ".shadow") in
          output := Some out;
          at_exit (fun () -> close_out out);
          out in
      let str_of_loc loc =
        let file_name, start_line, start_bol, start_off,
            stop_line, stop_bol, stop_off, _ = Ast.Loc.to_tuple loc in
        Printf.sprintf "\"%s\" %d %d %d \"%s\" %d %d %d"
          file_name start_line start_bol start_off
          file_name stop_line stop_bol stop_off
      in
      Printf.fprintf output "%s\nshadow(\n  %s %s\n)\n" (str_of_loc loc) id (str_of_loc shadow)
  end

  (* warning choice *)
  module Warn = struct
    (* default behavior : stderr *)
    let warn_in_file = ref false
    let () =
      Options.add "-to-file" (Arg.Set warn_in_file)
        "write warning for foo.ml in foo.ml.shadow";
      Options.add "-to-stderr" (Arg.Clear warn_in_file)
        "write warning to stderr"

    let print_warning loc id shadow =
      if !warn_in_file
      then Warn_file.print_warning loc id shadow
      else Warn_stderr.print_warning loc (Printf.sprintf "shadowing binding '%s' from %s" id
             (Ast.Loc.to_string shadow))
  end

  let check = object (self : 'self)
    inherit Ast.fold as super

    val bound_vars = []
    val verbose = true

    method bound_vars = bound_vars
    method set_bound_vars vars = {< bound_vars = vars >}
    method set_verbosity v = {< verbose = v >}

    (* after some local processing we often have to rewind the bound
       vars to a former state *)
    method rewind state = self#set_bound_vars state#bound_vars

    method add_binding id loc =
      if verbose && List.mem_assoc id bound_vars then
        Warn.print_warning loc id (List.assoc id bound_vars);
      self#set_bound_vars ((id, loc) :: bound_vars)

    method patt = function
    | <:patt@loc< ? $arg$ : ($patt$ = $def$) >> ->
      (* we first try the default value for shadowing,
         then add the name as binding and check the pattern *)
      let self = (self#expr def)#rewind self in
      let self = if arg <> "" then self#add_binding arg loc else self in
      self#patt patt
    | <:patt@loc< $lid:id$ >> -> self#add_binding id loc
    | <:patt< $a$ | $b$ >> ->
        ((self#patt a)#rewind self)#patt b
    | other -> super#patt other

    method match_case = function
    | <:match_case< $a$ | $b$ >> ->
      ((self#match_case a)#rewind self)#match_case b
    | other -> super#match_case other

    (* process (.. and ..) bindings, recursive or not *)
    method process_bindings recursive bindings =
      let patts, exprs =
        let rec fold patts exprs = function
        | <:binding< $p$ = $e$ >> -> (p :: patts), (e :: exprs)
        | <:binding< $a$ and $b$ >> ->
            let patts', exprs' = fold patts exprs a in
            fold patts' exprs' b
        | other -> List.rev patts, List.rev exprs in
        fold [] [] bindings in
      let process_patts recursive =
        List.fold_left
          (fun obj patt ->
             if recursive then obj#patt patt
             else begin
               (* if the bindings are not mutually recursive,
                  they do not shadow each other :
                  we first test against self (the outer context)
                  then silently build the new object *)
               ignore (self#patt patt);
               ((obj#set_verbosity false)#patt patt)#set_verbosity verbose
             end) in
      let process_exprs init_obj =
        List.fold_left
          (* expression bindings are local : we rewind to init_obj *)
          (fun obj expr -> (obj # expr expr)#rewind init_obj)
          init_obj in

      let with_paths = process_patts recursive self patts in
      if recursive then process_exprs with_paths exprs
      else
        (* if the bindings are not mutually recursive, they
           can't be shadowed inside the expressions :
           we process the expressions in the outer context (self),
           then rewind to the current context (with_path) *)
        (process_exprs self exprs)#rewind with_paths
      
    method expr = function
    | <:expr< let $bi$ in $e$ >> -> (self#process_bindings false bi)#expr e
    | <:expr< let rec $bi$ in $e$ >> -> (self#process_bindings true bi)#expr e
    | other -> super#expr other

    method str_item = function
    | <:str_item< value $bi$ >> -> self#process_bindings false bi
    | <:str_item< value rec $bi$ >> -> self#process_bindings true bi
    | other -> super#str_item other
  end

  let () = register_str_item_filter
    (fun str_item -> 
       let _ = check#str_item str_item in
       str_item)
end

let module M = Register.AstFilter(Id)(Make) in ()
