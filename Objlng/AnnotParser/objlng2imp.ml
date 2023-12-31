open Objlng

let tr_op: binop -> Imp.binop = function
  | Add -> Add
  | Mul -> Mul
  | Lt  -> Lt

let default_value: typ -> Imp.expression = function
  | TInt -> Cst 0
  | TBool -> Bool true
  | TClass n -> failwith "not supported yet"
  | TArray t -> Alloc(Cst 4)
  | _ -> assert false

(* types for various environments *)
module Env = Map.Make(String)

let add2env l env =
  List.fold_left (fun env (x, t) -> Env.add x t env) env l

(* translate a program *)
let translate_program (p: typ program) =
  let field_offset (f: string) (cdef: typ class_def): Imp.expression =
    let rec sub_field_offset (f: string) (cdef: typ class_def): int * bool =
      let inheritance = Objlng.has_get_parent cdef p.classes in 
      let parent_offset =
        if fst inheritance then sub_field_offset f (snd inheritance)
        else (4, false)
      in
      let (offset, found) = if snd parent_offset then (0, true) else
        List.fold_left (
          fun pair field -> if snd pair || fst field = f then (fst pair, true) else (fst pair+4, false)
        ) (0, false) cdef.fields
      in (offset+fst parent_offset, found)
    in Cst (fst (sub_field_offset f cdef))
  in
  let method_offset (metName: string) (cdef: typ class_def) (tr_e: Imp.expression): Imp.expression =
    (* search method offset and searches in parent descriptors if not found:
       for redefined methods, the redefined version is in the current descriptor so will be found before the previous version
       
     *)
    let rec sub_method_offset (metName: string) (cdef: typ class_def) (tr_e: Imp.expression): Imp.expression =
      let (offset, found) = List.fold_left (
        fun pair (met: typ function_def) -> if snd pair || met.name = metName then (fst pair, true) else (fst pair+4, false)
      ) (4, false) cdef.methods in
      if found then
        Binop(Add, Deref(tr_e), Cst offset)
      else
        let inheritance = has_get_parent cdef p.classes in
        if fst inheritance then
          sub_method_offset metName (snd inheritance) (Deref(tr_e))
        else
          failwith "method not found"
    in Deref(sub_method_offset metName cdef tr_e)
  in
  (* translate a function *)
  let tr_fdef (fdef: typ function_def): Imp.function_def = 
    let rec typ2byt: typ -> int = function
      | TInt      -> 4
      | TBool     -> 1
      | TClass x ->
        let def = find_class (TClass x) p.classes in
        List.fold_left (fun cpt p -> cpt + typ2byt (snd p)) 0 def.fields
      | TArray t  -> typ2byt t
      | TVoid     -> raise(Invalid_argument "TVoid isn't a valid type")
    in
    (* translation of an expression *)
    let rec tr_expr (te: typ expression): Imp.expression = match te.expr with
      | Cst n  -> Cst n
      | Bool b -> Bool b
      | Var x  -> Var x
      | Binop(op, e1, e2) -> Binop(tr_op op, tr_expr e1, tr_expr e2)
      | Call(x, l) -> Call(x, List.map tr_expr l)
      | This -> Var ("this")
      | Super -> Var ("super")
      | NewTab(t, e) -> Alloc(Binop(Mul, tr_expr e, Cst (typ2byt te.annot)))
      | Read m -> Deref(tr_mem m)
      | MCall(e2, x2, l) ->
        let tr_e = tr_expr e2 in
        let tr_e_arg: Imp.expression = match tr_e with
          | Imp.Var x -> if x = "super" then Var "this" else Var x
          | smth -> smth        
        in
        let clsse = find_class e2.annot p.classes in begin
          match clsse.parent with
            | Some parent -> DCall(method_offset x2 clsse tr_e, tr_e_arg :: Var (parent^"_descr_ptr") :: List.map tr_expr l)
            | None -> DCall(method_offset x2 clsse tr_e, tr_e_arg :: List.map tr_expr l)
          end
      | Instanceof(obj, c) -> Bool(Objlng.is_instance_of obj.annot c p.classes)
      | _ -> failwith ("Expr not caught: "^expr_to_string te)
    and tr_mem: typ mem -> Imp.expression = function
      | Atr(e, x) -> Binop(Add, tr_expr e, field_offset x (find_class e.annot p.classes))
      | Arr(e1, e2) -> Imp.array_access (tr_expr e1) (tr_expr e2)
    in
    let create_instance (cdef: typ class_def): Imp.expression =
      let rec sum_fields_size (cdef: typ class_def): int =
        let fields_size = List.fold_left (fun cpt field -> cpt + 4) 0 cdef.fields in
        let inheritance = has_get_parent cdef p.classes in 
        if fst inheritance then
          fields_size + sum_fields_size (snd inheritance)
        else
          fields_size
      in Alloc(Cst (4+sum_fields_size cdef))
    in
    let new_class_creation x1 x2 args =
      let cdef = find_class (TClass x2) p.classes in
        let instance = create_instance cdef in
        let this_super: Imp.expression list = match cdef.parent with
          | Some parent -> Var x1 :: Var (parent^"_descr_ptr") :: []
          | None -> Var x1 :: []
        in 
        Imp.Seq([
          Imp.Set(x1, instance);
          Write(Var x1, Var (x2^"_descr"));
          Expr(Call(x2 ^ "_constructor", this_super @ args))
        ])
    in
    (* translation of instructions *)
    let rec tr_seq s = List.map tr_instr s
    and tr_instr: typ instruction -> Imp.instruction = function
      | Putchar e     -> Putchar(tr_expr e)
      | If(e, s1, s2) -> If(tr_expr e, tr_seq s1, tr_seq s2)
      | While(e, s) -> While(tr_expr e, tr_seq s)
      | Return e -> Return(tr_expr e)
      | Expr e -> Expr(tr_expr e)
      | Set(x1, e) -> begin match e.expr with
        | New(x2, l) -> new_class_creation x1 x2 (List.map tr_expr l)
        | Cast(obj, c) ->
          let cdefChild = find_class (TClass c) p.classes in
          if is_instance_of obj.annot c p.classes then (* its an upcast *)
            let args = List.fold_right (
              fun (field, _) argsL ->
                Imp.Deref(tr_mem (Atr(obj, field))) :: argsL
              ) cdefChild.fields [] in
            new_class_creation x1 c args
          else (* its a downcast *)
            let cdefParent = find_class obj.annot p.classes in 
            let args =
              List.fold_right (
                fun (fieldP, _) argsL ->
                  Imp.Deref(tr_mem (Atr(obj, fieldP))) :: argsL
                ) cdefParent.fields [] @
              List.fold_right (
                fun (_, t) argsL ->
                  default_value t :: argsL                    
                ) cdefChild.fields []
            in new_class_creation x1 c args
        | Call(x2, l) -> Set(x1, Call(x2, List.map tr_expr l))
        | MCall(e2, x2, l) ->
          let tr_e = tr_expr e2 in
          let clsse = find_class e2.annot p.classes in begin
          match clsse.parent with
            | Some parent -> Set(x1, DCall(method_offset x2 clsse tr_e, tr_e :: Var (parent^"_descr_ptr") :: List.map tr_expr l))
            | None ->
              Set(x1, DCall(method_offset x2 clsse tr_e, tr_e :: List.map tr_expr l))
          end
        | _ -> Set(x1, tr_expr e)
      end
      | Write(m, e) -> Write(tr_mem m, tr_expr e)
    in
    { Imp.name = fdef.name; 
    params = List.map fst fdef.params; 
    locals = List.map fst fdef.locals; 
    code = tr_seq fdef.code;
    }
  in
  (* modifying methods in class_definitions:
      - changing the constructor (adding "this" param)
      - changing method name (adding class name)
      - adding "super" parameter in case it's an extension
    *)
  let rectify_methods (cdef: typ class_def): typ class_def =
    let mets = List.map (
      fun (met: typ function_def): typ function_def ->
        let parameters = match cdef.parent with
          | Some parent -> ("this", TClass cdef.name)::("super", TClass parent)::met.params
          | None -> ("this", TClass cdef.name)::met.params
        in
        {
          name=cdef.name^"_"^met.name;
          params=parameters;
          locals=met.locals;
          code=met.code;
          return=met.return;
        }
    ) cdef.methods
    in {cdef with methods=mets}
  in
  (* translating to imp class_definitions methods *)
  let tr_cdef_methods (cdef: typ class_def): Imp.function_def list =
    List.map tr_fdef (rectify_methods cdef).methods
  in
  (* creating a class descriptor: 
    - we choose to make it as a function called in main 
    - contains only explicitely defined methods (inheritated methods are accessed through parent class_descriptor)
    - contains also the declaration of a pointer to the pointer of the class descriptor for super
   *)
  let tr_c_descriptor (cdef: typ class_def): Imp.function_def =
    let descr_name: string = cdef.name^"_descr" in
    let inheritance = has_get_parent cdef p.classes in 
    let methods =
      List.mapi (
        fun i (met: typ function_def) -> Imp.Write(Binop(Add, Var descr_name, Cst ((i+1)*4)), Addr(met.name)))
        (rectify_methods cdef).methods
    in
    let parent: Imp.expression = if fst inheritance then Var((snd inheritance).name^"_descr") else Cst 0
    in
    let code =
      match cdef.parent with
        | Some p -> 
          [
            Imp.Set(p^"_descr_ptr", Imp.Alloc(Cst 4));
            Imp.Write(Var (p^"_descr_ptr"), Var (p^"_descr"));
            Imp.Set(descr_name, Imp.Alloc(Cst ((List.length cdef.methods+1) * 4)));
            Imp.Write(Var descr_name, parent)
          ] @ methods
        | None ->
          [
            Imp.Set(descr_name, Imp.Alloc(Cst ((List.length cdef.methods+1) * 4)));
            Imp.Write(Var descr_name, parent);
          ] @ methods
    in
    { name=cdef.name^"_descriptor"; params=[]; locals=[]; code=code }
  in
  (* function for addind class descriptors call in main *)
  let tr_functions (fdef: typ function_def) (c_descrs: Imp.function_def list): Imp.function_def =
    if fdef.name = "main" then
      let seq = List.map (fun (c_descr: Imp.function_def) -> Expr({annot=TVoid; expr=Call(c_descr.name, [])})) c_descrs
      in
      tr_fdef {fdef with code=seq @ fdef.code}
    else
      tr_fdef fdef
  in
  let class_descriptors = List.map tr_c_descriptor p.classes
  in
  let class_global_var classes =
    List.fold_left (fun l c ->
        match c.parent with
          | Some parent -> (c.name^"_descr") :: (parent^"_descr_ptr") :: l
          | None -> (c.name^"_descr") :: l
      ) [] classes
  in
  { Imp.globals =
      List.map fst p.globals @ class_global_var p.classes;
    Imp.functions = class_descriptors @ List.flatten (List.map tr_cdef_methods p.classes) @ List.map (fun f -> tr_functions f class_descriptors) p.functions; }