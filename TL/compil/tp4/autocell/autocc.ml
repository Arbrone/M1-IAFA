(*
 * autocell - AutoCell compiler and viewer
 * Copyright (C) 2021  University of Toulouse, France <casse@irit.fr>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *)

open Common
open Printf
open Quad

let _ =
	let dump_ast = ref false in
	let one = ref false in

	let make_field f (id, (num, (lo, hi))) =
		f (JRECORD [
			("name", JSTR id);
			("num", JINT num);
			("lo", JINT lo);
			("hi", JINT hi)
		]) in

	let compile lexbuf name =
		try
			let (flds, stmt) = Parser.program Lexer.token lexbuf in
			if !dump_ast then
				(Ast.print_stmt "" stmt; printf "\n"; exit 0)
			else
				let metas = [
					("source", JSTR name);
					("fields", JARRAY (fun f -> 
						List.iter (make_field f) flds
					))
				] in
				(metas, Comp.compile flds stmt)
		with
		| LexerError msg ->
			source_fatal lexbuf name msg
		| Parsing.Parse_error ->
			source_fatal lexbuf name "syntax error"
		| SyntaxError msg ->
			source_fatal lexbuf name msg in

	let rec escape i s =
		let l = String.length s in
		if i >= l then s else
		if s.[i] <> '"' then escape (i+1) s else
		(String.sub s 0 i) ^ "\\\"" ^ (escape 0 (String.sub s (i+1) (l-i-1))) in

	let save metas quads output =
			List.iter (fun (k, v) -> fprintf output "\t.meta %s \"%s\"\n" k (escape 0 (json_to_string v))) metas;
			output_prog output quads in
	
	let compile_path path =
		one := true;
		let (metas, qs) =
			try
				let in_channel = open_in path in
				let (metas, qs) = compile (Lexing.from_channel in_channel) path in
				close_in in_channel;
				(metas, qs)
			with Sys_error msg ->
				fatal_error (sprintf "ERROR: cannot load %s: %s\n" path msg) in
		let out_path = set_suffix path ".auto" ".s" in
		try
			let out_channel = open_out out_path in
			save metas qs out_channel;
			fprintf stderr "Assembly saved to %s\n" out_path;
			close_out out_channel
		with Sys_error msg ->
			fprintf stderr "ERROR: cannot save %s: %s\n" out_path msg in

	Arg.parse
		[
			("-ast", Set dump_ast, "Dump the AST.")
		]
		compile_path
		"autocc [<file1>.auto...]";

	if not !one then
		let (metas, quads) = compile (Lexing.from_channel stdin) "<stdin>" in
		save metas quads stdout
