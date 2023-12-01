let is_digit c =
  c >= '0' && c <= '9';;

let char_to_int c =
  int_of_char c - int_of_char '0';;

(**let rec find_numbers line lo hi =
if lo > hi then
    []
  else if is_digit line.[lo] then
    line.[lo] :: find_numbers line (lo + 1) hi
  else
    find_numbers line (lo + 1) hi;;
*)

let nums = [
  ("zero", "0"); ("one", "1"); ("two", "2"); ("three", "3");
  ("four", "4"); ("five", "5"); ("six", "6"); ("seven", "7");
  ("eight", "8"); ("nine", "9")
]

let match_words_at_index index query word =
  let word_len = String.length word in
  let query_len = String.length query in 
  if index + word_len > query_len then false
  else
    String.sub query index word_len = word


let rec check_num_match numbers index query =
  match numbers with
  | (word, value) :: tail ->
      if match_words_at_index index query word then Some value
      else check_num_match tail index query
  | _ -> None
  

let rec find_first_val line lo hi =
  if lo < 0 || lo > (String.length line - 1) then
    None
  else if is_digit line.[lo] then
    Some (String.make 1 line.[lo])
  else
    match check_num_match nums lo line with
      | Some value -> Some value
      | None ->
        if lo < hi then
          find_first_val line (lo + 1) hi
        else
          find_first_val line (lo - 1) hi

let read_file_by_line filename = 
  let sum = ref 0 in
  let channel = open_in filename in 
  try
    while true do
      let line = input_line channel in
      let a = find_first_val line 0 (String.length line - 1) in
      let b = find_first_val line (String.length line - 1) 0 in
      
      match (a,b) with
        | (Some aValue, Some bValue) ->
          let num_str = aValue ^ bValue in
          let num = int_of_string num_str in
          sum := !sum + num;
          Printf.printf "The sum is: %d\n" !sum;
        | _ -> () 
      done;
      
      Printf.printf "The integer is: %d\n" !sum
  with End_of_file ->
    close_in channel;;

let () =
  let filename = "./b.txt" in
  read_file_by_line filename
