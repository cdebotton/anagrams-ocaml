open Printf

module StringHash = struct
  type t = string
  let equal = (=)
  let hash = Hashtbl.hash
end

module Table = Hashtbl.Make (StringHash)

let file = "resources/wordlist.txt"

let load_file f =
  let ic = open_in f in
  try
    let n = in_channel_length ic in
    let s = Bytes.create n in
    really_input ic s 0 n;
    close_in ic;
    (s |> Bytes.to_string)
  with e ->
    close_in_noerr ic;
    raise e

let split_lines = Str.split (Str.regexp "\n")

let group_by_anagrams list =
  let size = List.length list in
  let table = Table.create size in
  let rec aux acc = function
    | [] -> acc
    | hd::tl when (String.length hd) >= 3 ->
      let lowercase_word = String.lowercase_ascii hd in
      let w = Str.global_replace (Str.regexp "[^a-z]") "" lowercase_word in
      (* Need to sort stirng and create hash
         set key of hash in table to a list of the anagrams
       *)
      aux acc tl
    | _hd::tl -> aux acc tl in
  aux table list

let () =
  file
  |> load_file
  |> split_lines
  |> group_by_anagrams
  (* Pass in method that takes hash table, provides a read line,
     generates the hash key for the user input and returns
     the list of anagrams if it exists
    *)
  |> ignore
  