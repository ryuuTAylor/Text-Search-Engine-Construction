module type E = sig
  type idx

  val index_of_dir : string -> idx
  val words : idx -> string list
  val to_list : idx -> (string * string list) list
  val or_not : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
end

module Make =
functor
  (S : DictionarySet.Set with type Elt.t = string)
  (D : Dictionary.Dictionary with type Key.t = string and type Value.t = S.t)
  ->
  struct
    (* Begin: Do not change any of the provided code below. *)

    type idx = D.t
    (** An [idx] maps from strings to sets of strings. *)

    (** [word_regexp] is a regular expression for parsing words. It matches
        strings that begin and end with a _boundary character_, which is any
        lowercase letter, uppercase letter, or digit. In between the boundary
        characters there may be any number and kind of other characters. There
        are some weird corner cases resulting from this definition of words, but
        it's relatively simple, and it gets many common cases right.*)
    let word_regexp =
      Str.regexp "\\([A-Za-z0-9]+.*[A-Za-z0-9]+\\)\\|[A-Za-z0-9]+"

    (** [word_of_preword pw] is the lower-case _word_ corresponding to _preword_
        [pw], if any. A preword is a sequence of non-blank-space characters. See
        [word_regexp] for the definition of _word_. *)
    let word_of_preword pw =
      let open Str in
      try
        let _ = search_forward word_regexp pw 0 in
        Some (String.lowercase_ascii (matched_string pw))
      with Not_found -> None

    (** [words_of_prewords] converts a list of prewords to a list of words. *)
    let words_of_prewords pws =
      (* we do this with a custom function, so that we can simultaneously map
         and filter, tail recursively, while not allocating any more memory than
         needed, and so that we hopefully don't trigger too much GC when
         processing large files *)
      let rec loop acc = function
        | [] -> acc
        | h :: t -> (
            match word_of_preword h with
            | None -> loop acc t
            | Some w -> loop (w :: acc) t)
      in
      loop [] pws

    (** [blankspace_regexp] is a regular expression that matches _blank space_,
        which are spaces and tabs. *)
    let blankspace_regexp = Str.regexp "[ \t]+"

    (** [words_in_line line] is a list of the words parsed from a line of text. *)
    let words_in_line line =
      line
      |> (* extract prewords *) Str.split blankspace_regexp
      |> (* convert to words *) words_of_prewords

    (** [safe_input_line] inputs a line from an input channel, without raising
        an exception. *)
    let safe_input_line chan =
      try Some (input_line chan) with End_of_file -> None

    (** [insert_bindings words file dict] iterates over every word [w] in
        [words], inserting [file] into the set [s] to which [dict] binds [w]. *)
    let insert_bindings words file dict =
      let wordset word dict =
        match D.find word dict with
        | None -> S.empty
        | Some s -> s
      in
      let insert_binding dict word =
        let s = wordset word dict in
        let s' = S.insert file s in
        D.insert word s' dict
      in
      List.fold_left insert_binding dict words

    (** [index_of_file path name d] indexes the file found at [path], adding
        bindings for its words to [name] in dictionary [d]. *)
    let index_of_file filepath filename dict =
      let chan = open_in filepath in
      let rec loop acc =
        match safe_input_line chan with
        | None -> acc
        | Some line ->
            let word_list = words_in_line line in
            loop (insert_bindings word_list filename acc)
      in
      let dict' = loop dict in
      close_in chan;
      dict'

    let index_of_dir d =
      let safe_readdir dh =
        try Some (Unix.readdir dh) with End_of_file -> None
      in
      let my_opendir d = try Unix.opendir d with _ -> raise Not_found in
      let is_txt_file f = Filename.check_suffix f ".txt" in
      let dh = my_opendir d in
      let rec loop acc =
        match safe_readdir dh with
        | Some f ->
            if is_txt_file f then
              let fp = d ^ Filename.dir_sep ^ f in
              loop (index_of_file fp f acc)
            else loop acc
        | None -> acc
      in
      let dict = loop D.empty in
      Unix.closedir dh;
      dict

    (* End: Do not change any of the provided code above. *)

    let to_list idx = List.map (fun (x, y) -> (x, S.to_list y)) (D.to_list idx)
    let words idx = List.map (fun (x, y) -> x) (D.to_list idx)

    let or_not idx ors nots =
      let a = List.filter (fun (x, y) -> List.mem x ors) (D.to_list idx) in
      let b = List.filter (fun (x, y) -> List.mem x nots) (D.to_list idx) in
      let a' = List.map (fun (x, y) -> y) a in
      let b' = List.map (fun (x, y) -> y) b in
      let a'' = List.fold_left (fun x acc -> S.union x acc) S.empty a' in
      let b'' = List.fold_left (fun x acc -> S.union x acc) S.empty b' in
      S.(difference a'' b'' |> to_list)

    let and_not idx ands nots =
      let a = List.filter (fun (x, y) -> List.mem x ands) (D.to_list idx) in
      let b = List.filter (fun (x, y) -> List.mem x nots) (D.to_list idx) in
      let a' = List.map (fun (x, y) -> y) a in
      let b' = List.map (fun (x, y) -> y) b in
      let c' = List.map (fun (x, y) -> y) (D.to_list idx) in
      let c'' = List.fold_left (fun x acc -> S.union x acc) S.empty c' in
      let a'' = List.fold_left (fun x acc -> S.intersect x acc) c'' a' in
      let b'' = List.fold_left (fun x acc -> S.union x acc) S.empty b' in
      S.(difference a'' b'' |> to_list)
  end
