let translate_program (p: Objlng.program) =
  let tr_fdef = failwith "not implemented" 

in
  { Imp.globals = List.map fst p.globals;
    functions = List.map tr_fdef p.functions; }