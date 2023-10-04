open Format

let () =
  let file = Sys.argv.(1) in
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let prog = Simpparser.program Simplexer.token lb in
  close_in c;
  let imp = Simp2imp.translate_program prog in
  let imp_output_file = (Filename.chop_suffix file ".simp") ^ ".imp" in
  let imp_out = open_out imp_output_file in
  let imp_outf = formatter_of_out_channel imp_out in
  Imppp.print_program imp_outf imp;
  pp_print_flush imp_outf ();
  close_out imp_out;
  let asm = Imp2mips.translate_program imp in
  let output_file = (Filename.chop_suffix file ".simp") ^ ".asm" in
  let out = open_out output_file in
  let outf = formatter_of_out_channel out in
  Mips.print_program outf asm;
  pp_print_flush outf ();
  close_out out;
  exit 0
