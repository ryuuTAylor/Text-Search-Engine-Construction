open Dictionary

module Make : DictionaryMaker =
functor
  (K : KeySig)
  (V : ValueSig)
  ->
  struct
    module Key = K
    module Value = V

    type key = K.t
    type value = V.t

    type t = (key * value) list
    (** AF: The association list [(k1, v1); (k2, v2); ...; (kn, vn)] represents
        the dictionary that maps k1, ..., kn to v1, ..., vn accordingly. The
        empty association list [] represents an empty dictionary with no
        key-value pair. RI: The association list is always sorted in an
        increasing order of the keys. No duplicate keys are allowed. *)

    let rep_ok (d : t) : t =
      let rec is_sorted = function
        | [] | [ _ ] -> true
        | (k1, _) :: (k2, v2) :: tl ->
            Key.compare k1 k2 = LT && is_sorted ((k2, v2) :: tl)
      in
      let rec has_duplicates = function
        | [] | [ _ ] -> false
        | (k, _) :: tl ->
            List.exists (fun (k', _) -> k' = k) tl || has_duplicates tl
      in
      if is_sorted d && not (has_duplicates d) then d
      else failwith "RI violated"
      [@@coverage off]

    let rep_ok = Fun.id [@@coverage off]
    let empty : t = []

    let is_empty (d : t) : bool =
      let d = rep_ok d in
      d = empty

    let size (d : t) : int =
      let d = rep_ok d in
      List.length d

    let rec insert (k : key) (v : value) (d : t) : t =
      let d = rep_ok d in
      match d with
      | [] -> [ (k, v) ]
      | (k', v') :: t ->
          if Key.compare k k' = LT then (k, v) :: (k', v') :: t
          else if Key.compare k k' = EQ then (k, v) :: t
          else (k', v') :: insert k v t

    let rec remove (k : key) (d : t) : t =
      let d = rep_ok d in
      match d with
      | [] -> []
      | (k', v') :: t ->
          if Key.compare k k' = EQ then t else (k', v') :: remove k t

    let rec find (k : key) (d : t) : value option =
      let d = rep_ok d in
      match d with
      | [] -> None
      | (k', v') :: t -> if Key.compare k k' = EQ then Some v' else find k t

    let rec member (k : key) (d : t) : bool =
      let d = rep_ok d in
      match d with
      | [] -> false
      | (k', _) :: t -> Key.compare k k' = EQ || member k t

    let to_list (d : t) : (key * value) list = d

    let fold (f : key -> value -> 'acc -> 'acc) (init : 'acc) (d : t) =
      let d = rep_ok d in
      List.fold_left (fun init (k, v) -> f k v init) init d

    let to_string (d : t) =
      let d = rep_ok d in
      List.fold_left
        (fun acc (a, b) ->
          acc ^ K.to_string a ^ " maps to " ^ V.to_string b ^ "; ")
        "" d
  end
