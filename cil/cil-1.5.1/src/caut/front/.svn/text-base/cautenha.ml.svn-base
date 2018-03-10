(** cautprep.ml
	Author: Jiang Siyuan
	Date:4/25/2010
	Description: enhancement features, including replacing scanf
*)
module E = Errormsg

open Cil
open String

exception Fail of string

class repsca_visitor (fi : fundec) = object (self)
	inherit nopCilVisitor

	(* input: int *p --> output int val (= *p) *)
	method private setTemp (e1: exp) : exp = 
		let t = match (typeOf e1) with
			(* TArray can't be assigned *)
			| TPtr (tt, att) -> makeTempVar fi tt
			| _ -> ignore(E.error "\nWarning: don't know how to do with other typs in SCANF");
				let tt = TInt(IInt,[]) in
				makeTempVar fi tt
		in
		self#queueInstr [Set(var t, Lval(Mem(e1),NoOffset), !currentLoc)];
		Lval(var t)

	method private dealScanfFormals (exps : exp list) : exp list=
		try (
			let e = List.hd exps in
			ignore(E.log "\n%a\n" d_exp e);
			[self#setTemp e] @ ( self#dealScanfFormals (List.tl exps) )
		)
		with _ -> []

	method vinst (ist : instr) =
		match ist with
		| Set _ -> DoChildren
		| Call (lval_o, c_exp, exp_l, c_loc)->
			let flag = (match c_exp with
				|Const _ -> ignore(E.log "\nError: Function's name is a Const!"); false
				|Lval (l_host,l_off) -> 
					(match l_host with
						| Var (l_vi) ->
							if(l_vi.vname = "scanf") then begin
								(* HERE: Change the scanf function to __CAUT_INPUT *)
								ignore(l_vi.vname <- "__CAUT_INPUT");
								true 
							end else false
						| _ -> false
					)
				|_ -> ignore(E.log "\nError: Functions's name is not a Lval!");false
			) in
			if flag = true then
			begin
				ignore (E.log "\n%a\n" d_instr ist);
				try(
					let instrs = ref([]:instr list) in
					List.iter (fun x -> instrs:=!instrs@[Call(None,c_exp,[x],c_loc)]) (self#dealScanfFormals (List.tl exp_l));
					ChangeTo !instrs
				)
				with _ -> ignore(E.log "\nError: Functions's name is not a Lval!");DoChildren
			end else DoChildren
		| Asm _ -> DoChildren
end

let doGlobal = function 
	GFun(fi, _) ->
		let repscaVst = new repsca_visitor fi in
		fi.sbody <- visitCilBlock repscaVst fi.sbody;
	| _ -> ()

let feature : featureDescr = 
  { fd_name = "cautenha";
    fd_enabled = ref false;
    fd_description = "CAUT Front-end : caut enhancement : replacing specific functions with corresponding codes";
    fd_extraopt = [];
    fd_doit = (function f -> iterGlobals f doGlobal);
    fd_post_check = true
  } 
