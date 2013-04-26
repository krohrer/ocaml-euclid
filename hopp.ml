open Format

let pp_open_lbox fmt prefix =
  pp_print_string fmt prefix;
  pp_open_box fmt 0

let pp_open_lhbox fmt prefix =
  pp_print_string fmt prefix;
  pp_open_hbox fmt ()

let pp_open_lvbox fmt prefix =
  pp_print_string fmt prefix;
  pp_open_vbox fmt 0

let pp_open_lhvbox fmt prefix =
  pp_print_string fmt prefix;
  pp_open_hvbox fmt 0

let pp_open_lhovbox fmt prefix =
  pp_print_string fmt prefix;
  pp_open_hvbox fmt 0

let pp_close_lbox fmt postfix =
  pp_close_box fmt ();
  pp_print_string fmt postfix

let pp_print_separator fmt sep =
  pp_print_string fmt sep;
  pp_print_space fmt ()

let abstract_to_string a =
  ignore a; "<abstract>"

let pp_print_abstract fmt a =
  pp_print_string fmt (abstract_to_string a)

let pp_print_quoted fmt s =
  fprintf fmt "%S" s

let pp_print_labeled_field label pr fmt a =
  pp_open_box fmt 2;
  pp_print_string fmt label;
  pp_print_separator fmt " =";
  pr fmt a;
  pp_close_box fmt ()

let pp_print_list pr fmt l =
  pp_open_box fmt 1;
  begin
    pp_print_string fmt "[";
    let rec loop =
      function
	| [] -> ()
	| [x] -> pr fmt x
	| x::rest ->
	    pr fmt x;
	    pp_print_string fmt ";";
	    pp_print_space fmt ();
	    loop rest
    in
      loop l;
      pp_print_string fmt "]";
  end;
  pp_close_box fmt ()

let pp_print_option pr fmt o =
  pp_open_box fmt 1;
  begin
    match o with
	None -> 
	  pp_print_string fmt "None"
      | Some v ->
	  pp_print_string fmt "Some ";
	  pr fmt v
  end;
  pp_close_box fmt ()

let pp_print_array pr fmt a =
  pp_open_box fmt 2;
  begin
    pp_print_string fmt "[|";
    let n = Array.length a in
      if n > 0 then
	pr fmt a.(0);
      for i = 1 to n-1 do
	pp_print_string fmt ";";
	pp_print_space fmt ();
	pr fmt a.(i)
      done
  end;
  pp_print_string fmt "|]";
  pp_close_box fmt ()
	  
let pp_make_to_string pr a =
  pr str_formatter a;
  flush_str_formatter ()
