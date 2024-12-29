[@@@ocaml.warning "-26"]
[@@@ocaml.warning "-27"]
[@@@ocaml.warning "-32"]
[@@@ocaml.warning "-33"]
[@@@ocaml.warning "-34"]
[@@@ocaml.warning "-69"]


let days_of_week = [ "Monday"; "Tuesday"; "Wednesday"; "Thursday"; "Friday"; "Saturday"; "Sunday" ]

type time_block = {
  day : string; (* Day of the week *)
  thing : string option;
  from : string;
  to_ : string;
  time : string;
  notes : string;
}


(* Parse a single row into time blocks for all days of the week *)
let parse_row row =
  (* Remove the first 3 columns (D15 onwards starts at column index 3) *)
  let remaining_cols = List.tl (List.tl (List.tl row)) in

  let rec aux acc cols day_idx =
    match cols with
    | thing :: from :: to_ :: time :: notes :: rest ->
      let block = {
        day = List.nth days_of_week day_idx;
        thing = if thing = "" then None else Some from;
        from;
        to_;
        time;
        notes;
      } in
      aux (block :: acc) rest (day_idx + 1)
    | [] -> List.rev acc
    | _ -> failwith "Malformed row: incomplete data for a day"
  in
  aux [] remaining_cols 0

let filter_block block =
  not (block.from = "" && block.to_ = "" && block.time = "" && block.notes = "" && Option.is_none block.thing)

(* Parse a TSV file into a list of time_block records *)
let parse_tsv file =
  let in_channel = open_in file in
  let rec read_lines acc current_line =
    try
      let line = input_line in_channel in
      if current_line >= 16 then
        (* let () = Printf.printf "Processing line %d\n%s\n\n\n" current_line line in *)
        let cols = String.split_on_char '\t' line in
        let blocks = parse_row cols in
        let filtered_blocks = List.filter filter_block blocks in
        read_lines (List.rev_append filtered_blocks acc) (current_line + 1)
      else
        (* let () = Printf.printf "ignoring line %d\n%s\n\n\n" current_line line in *)
        read_lines acc (current_line + 1)
    with End_of_file ->
      close_in in_channel;
      List.rev acc
  in
  read_lines [] 1

(* Walk a directory and find all .tsv files *)
let list_tsv_files dir =
  Array.fold_left
    (fun acc file ->
       let full_path = Filename.concat dir file in
       if Sys.file_exists full_path && Filename.check_suffix file ".tsv" then
         full_path :: acc
       else
         acc)
    [] (Sys.readdir dir)


(* Process a directory of TSV files *)
let process_directory dir =
  let files = list_tsv_files dir in
  List.fold_left
    (fun acc file ->
       let blocks = parse_tsv file in
       Printf.printf "Parsed %d blocks from %s\n" (List.length blocks) file;
       List.rev_append blocks acc)
    [] files


let group_blocks_by_day blocks =
  let table = Hashtbl.create 7 in
  List.iter (fun block ->
      let day = block.day in
      let existing_blocks = (
        try Hashtbl.find table day
        with Not_found -> []
      ) in
      Hashtbl.replace table day (block :: existing_blocks)
    ) blocks;
  table

let block_to_str block = 
  Printf.sprintf "Thing: %s, From: %s, To: %s, Time: %s, Notes: %s\n"
    (match block.thing with | None -> "N/A" |Some t -> t)  block.from block.to_ block.time block.notes

(* Example usage *)
let () =
  Printf.printf "\nhola about to scrape!!!!!!";
  let dir = "/Users/leo/dev/org/org_server/model_sheets/" in
  let all_blocks = process_directory dir in
  let blocks_by_day = group_blocks_by_day all_blocks in
  Hashtbl.iter (fun day blocks ->
      Printf.printf "\n\n\n\n\nDay %s\n" day;
      List.iter (fun block ->
          Printf.printf "%s" (block_to_str block);
        ) blocks
    ) blocks_by_day
(* List.iter *)
(*   (fun block -> *)
(*      Printf.printf *)
(*        "Thing: %s, From: %s, To: %s, Time: %s, Notes: %s\n" *)
(*        (match block.thing with | None -> "N/A" |Some t -> t)  block.from block.to_ block.time block.notes) *)
(*   all_blocks *)

