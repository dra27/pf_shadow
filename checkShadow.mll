{
(* ********************************************************************************************** *
 * MetaStack Solutions Ltd.                                                                       *
 * ********************************************************************************************** *
 * Objective Caml Shadowed Variable Warning Tool                                                  *
 * ********************************************************************************************** *
 * Copyright (c) 2008-9 MetaStack Solutions Ltd.                                                  *
 * All rights reserved.                                                                           *
 * ********************************************************************************************** *
 * Author: David Allsopp                                                                          *
 * 13-Sep-2008                                                                                    *
 * ********************************************************************************************** *
 * Redistribution and use in source and binary forms, with or without modification, are permitted *
 * provided that the following two conditions are met:                                            *
 *     1. Redistributions of source code must retain the above copyright notice, this list of     *
 *        conditions and the following disclaimer.                                                *
 *     2. Neither the name of MetaStack Solutions Ltd. nor the names of its contributors may be   *
 *        used to endorse or promote products derived from this software without specific prior   *
 *        written permission.                                                                     *
 *                                                                                                *
 * This software is provided by the Copyright Holder 'as is' and any express or implied           *
 * warranties, including, but not limited to, the implied warranties of merchantability and       *
 * fitness for a particular purpose are disclaimed. In no event shall the Copyright Holder be     *
 * liable for any direct, indirect, incidental, special, exemplary, or consequential damages      *
 * (including, but not limited to, procurement of substitute goods or services; loss of use,      *
 * data, or profits; or business interruption) however caused and on any theory of liability,     *
 * whether in contract, strict liability, or tort (including negligence or otherwise) arising in  *
 * any way out of the use of this software, even if advised of the possibility of such damage.    *
 * ********************************************************************************************** *)

exception Strange

module Location =
  struct
    type t = string * (int * int * int) * (int * int * int)
    let compare = Pervasives.compare
  end

module LocMap = Map.Make(Location)
}
(* ********************************************************************************************** *
 * file    - double-quoted strings containing escaped characters (empty strings allowed)          *
 * integer - strings of decimal digits (at least 1 digit)                                         *
 * eol     - platform-neutral end-of-line                                                         *
 * offset  - an offset read from a .offset or .annot file                                         *
 * ********************************************************************************************** *)
let file = ('"' (('\\' _) | [^ '\\' '"' ])* '"')
let integer = [ '0' - '9' ]+
let eol = ('\n' | "\r\n")
let offset = (file as file1) ' ' (integer as line1) ' ' (integer as bol1) ' ' (integer as offset1) ' '
             (file as file2) ' ' (integer as line2) ' ' (integer as bol2) ' ' (integer as offset2) eol

(* ********************************************************************************************** *
 * getTypeMap                                                                                     *
 * ********************************************************************************************** *
 * Entry point for reading a .annot file. Initially passed an empty LocMap, returns a map of      *
 * Location.t tuples to the string contents of the .annot file for that location. The lines for   *
 * each entry are reversed (as they are only used for comparison). [last] is used to track the    *
 * offset read before reading the type information and should initially be [None].                *
 * ********************************************************************************************** *)
rule getTypeMap last map = shortest
  (* ******************************************************************************************** *
   * offset. Note that this implementation does allow for an offset to be specified without any   *
   * corresponding information.                                                                   *
   * ******************************************************************************************** *)
  offset
   {let loc =
      if file1 <> file2
      then raise Strange
      else let start  = (int_of_string line1, int_of_string bol1, int_of_string offset1)
           and finish = (int_of_string line2, int_of_string bol2, int_of_string offset2)
           in
             (file1, start, finish)
    in
      getTypeMap (Some loc) map lexbuf}

| "type(" eol
   {match last with
      None     -> failwith "lexing: empty token"
      (* **************************************************************************************** *
       * Note that a single offset listed in the .annot file may have multiple type entries       *
       * (OCaml 3.11.0 coalesces these into one block where 3.10.2 and earlier would have         *
       * inserted multiple blocks).                                                               *
       * **************************************************************************************** *)
    | Some loc -> getTypeMap last (LocMap.add loc (getLines [] lexbuf) map) lexbuf}

   (* ******************************************************************************************* *
    * Other entries (OCaml 3.11+ compatibility)                                                   *
    * ******************************************************************************************* *)
| [^ '(' ]+ '(' eol
   {let _ = getLines [] lexbuf
    in
      getTypeMap last map lexbuf}

   (* ******************************************************************************************* *
    * Return the completed map.                                                                   *
    * ******************************************************************************************* *)
| eof
   {map}

(* ********************************************************************************************** *
 * getShadowList                                                                                  *
 * ********************************************************************************************** *
 * Entry point for reading a .shadow file. Initially passed an empty list, returns a list of      *
 * tuples giving identifier name, shadowing location and shadowed location respectively.          *
 * ********************************************************************************************** *)
and getShadowList acc = parse
  (* ******************************************************************************************** *
   * shadow entries                                                                               *
   * ******************************************************************************************** *)
  offset "shadow(" eol "  " ([^ ' ' ]+ as id) ' '
   {let loc =
      if file1 <> file2 || line1 <> line2 || bol1 <> bol2
      then raise Strange
      else let start  = (int_of_string line1, int_of_string bol1, int_of_string offset1)
           and finish = (int_of_string line2, int_of_string bol2, int_of_string offset2)
           in
             (file1, start, finish)
    in
      getShadowList ((id, loc, getTerminator lexbuf)::acc) lexbuf}

   (* ******************************************************************************************* *
    * Return the completed list.                                                                  *
    * ******************************************************************************************* *)
| eof
   {List.rev acc}

(* ********************************************************************************************** *
 * getLines                                                                                       *
 * ********************************************************************************************** *
 * Reads lines from the lexing buffer which are assumed to be prefixed with two spaces until a    *
 * line containing a single right bracket is encountered. The lines read are returned in reverse  *
 * order.                                                                                         *
 * ********************************************************************************************** *)
and getLines acc = parse
  "  " ([^ '\r' '\n' ]* as line) eol
   {getLines (line::acc) lexbuf}

| ')' eol
   {acc}

(* ********************************************************************************************** *
 * getTerminator                                                                                  *
 * ********************************************************************************************** *
 * Reads the shadowed location from a .shadow file and the closing right bracket and returns the  *
 * read Location.t                                                                                *
 * ********************************************************************************************** *)
and getTerminator = parse
  offset ')' eol
   {if file1 <> file2 || line1 <> line2 || bol1 <> bol2
    then raise Strange
    else let start  = (int_of_string line1, int_of_string bol1, int_of_string offset1)
         and finish = (int_of_string line2, int_of_string bol2, int_of_string offset2)
         in
           (file1, start, finish)}

{
(* ********************************************************************************************** *
 * Parse argument and open the channels                                                           *
 * ********************************************************************************************** *)
if Array.length Sys.argv <> 2
then Printf.eprintf "Usage: checkShadow {file}[.ml]\n\
                     \n\
                     Parses OCaml .annot and .shadow files and emits warnings for any shadowed\n\
                     identifiers where the type of shadowing instance and the type of shadowed\n\
                     instance are identical.\n"
else let (status, annotChannel, shadowChannel) =
       (* *************************************************************************************** *
        * If the argument has an extension then use it, otherwise add ".ml"                       *
        * *************************************************************************************** *)
       let source =
         let arg = Sys.argv.(1)
         in
           if List.exists (fun ext -> Filename.check_suffix arg ext) [".ml"; ".mll"; ".mly"]
           then arg
           else arg ^ ".ml"
       in
         if not (Sys.file_exists source) || (Unix.stat source).Unix.st_kind = Unix.S_DIR
         then begin
                Printf.eprintf "File not found\n";
                (`Error, stdin, stdin)
              end
         else let shadow = source ^ ".shadow"
              in
                (* ****************************************************************************** *
                 * If the .shadow file doesn't exist then either the user didn't compile with     *
                 * pf_shadow or there were no warnings to be written - in both instances we just  *
                 * silently terminate.                                                            *
                 * ****************************************************************************** *)
                if Sys.file_exists shadow
                then let annot = Filename.chop_extension source ^ ".annot"
                     in
                       if not (Sys.file_exists annot)
                       then begin
                              Printf.eprintf ".annot file not found - compile with -dtypes or -annot\n";
                              (`Error, stdin, stdin)
                            end
                       else try
                              let annotChannel = open_in annot
                              in
                                try
                                  (`Go, annotChannel, open_in shadow)
                                with Sys_error _ ->
                                  try
                                    Printf.eprintf "Could not open %s\n" shadow;
                                    close_in annotChannel;
                                    (`Error, stdin, stdin)
                                  with Sys_error _ -> (`Error, stdin, stdin)
                            with Sys_error _ ->
                              Printf.eprintf "Could not open %s\n" annot;
                              (`Error, stdin, stdin)
                else (`Idle, stdin, stdin)
     in
       match status with
         `Go ->
           let annotMap =
             try
               getTypeMap None LocMap.empty (Lexing.from_channel annotChannel)
             with e ->
               Printf.eprintf "Unexpected error %s while reading the .annot file\n" (Printexc.to_string e);
               exit 1
           and shadows =
             try
               getShadowList [] (Lexing.from_channel shadowChannel)
             with e ->
               Printf.eprintf "Unexpected error %s while reading the .shadow file\n" (Printexc.to_string e);
               exit 1
           in
             (* ********************************************************************************* *
              * Close the channels...                                                             *
              * ********************************************************************************* *)
             begin
               try
                 close_in annotChannel;
                 close_in shadowChannel
               with Sys_error _ -> ()
             end;
             (* ********************************************************************************* *
              * ... then filter the output.                                                       *
              * ********************************************************************************* *)
             let filtered =
               let f (id, loc, shadow) =
                 LocMap.find loc annotMap = LocMap.find shadow annotMap
               in
                 List.filter f shadows
             and printer (id, loc, shadow) =
               let formatLoc (file, (line, bol, offset1), (_, _, offset2)) =
                 Printf.sprintf "File %s, line %d, characters %d-%d" file line (offset1 - bol) (offset2 - bol)
               in
                 Printf.eprintf "<W> %s: shadowing binding '%s' from %s\n" (formatLoc loc) id (formatLoc shadow)
             in
               List.iter printer filtered;
               exit 0
       | `Idle -> exit 0
       | `Error -> exit 1
}
