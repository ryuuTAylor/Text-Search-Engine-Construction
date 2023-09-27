open Dictionary

module Make =
functor
  (K : KeySig)
  (V : ValueSig)
  ->
  struct
    module Key = K
    module Value = V

    type key = K.t
    type value = V.t

    (* AF: [Leaf] represents the empty dictionary. [Node (c, l, (k, v), r)] 
        represents the dictionary containing the key-value pair (k, v), as 
        well as all the elements of the dictionaries represented by [l] and [r].
     * RI: Both the BST Invariant and the Red-Black Tree Invaraints 
        (Both Local and Global) hold. *)

    type color =
      | Red
      | Black

    type t =
      | Leaf
      | Node of color * t * (key * value) * t

    (* Debug controls whether we should turn on rep_ok *)
    let debug = false

    (* Helper Methods of rep_ok which are justifiably turned [@@coverage off] *)

    let rep_ok d =
      let rec bst_inv = function
        | Leaf -> true
        | Node (_, l, (k, _), r) -> (
            match (l, r) with
            | Leaf, Node (_, _, (k', _), _) ->
                Key.compare k k' = LT && bst_inv r
            | Node (_, _, (k', _), _), Leaf ->
                Key.compare k' k = LT && bst_inv l
            | Node (_, _, (kl, _), _), Node (_, _, (kr, _), _) ->
                Key.compare kl k = LT
                && Key.compare k kr = LT
                && bst_inv r && bst_inv l
            | _ -> true)
      in
      let rec local_inv = function
        | Leaf -> true
        | Node (Red, Node (Red, _, _, _), _, _)
        | Node (Red, _, _, Node (Red, _, _, _)) -> false
        | Node (_, l, _, r) -> local_inv l && local_inv r
      in
      let rec bh_inv = function
        | Leaf -> 1
        | Node (c, l, _, r) ->
            let left_bh = bh_inv l in
            let right_bh = bh_inv r in
            if left_bh = right_bh then
              if c = Black then left_bh + 1 else left_bh
            else failwith "RI violated"
      in
      if debug then
        if local_inv d && bh_inv d >= 1 && bst_inv d then d
        else failwith "RI violated"
      else d
      [@@coverage off]

    let empty = Leaf

    let is_empty d =
      let d = rep_ok d in
      d = empty

    let rec size d =
      let d = rep_ok d in
      match d with
      | Leaf -> 0
      | Node (_, l, _, r) -> 1 + size l + size r

    let balance = function
      | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
      | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
      | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
      | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
          Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
      | a, b, c, d -> Node (a, b, c, d)

    let insert k v d =
      let d = rep_ok d in
      let rec ins = function
        | Leaf -> Node (Red, Leaf, (k, v), Leaf)
        | Node (color, a, (y, vy), b) ->
            if Key.compare k y = LT then balance (color, ins a, (y, vy), b)
            else if Key.compare k y = GT then balance (color, a, (y, vy), ins b)
            else Node (Red, a, (k, v), b)
      in
      match ins d with
      | Node (_, a, y, b) ->
          let d = rep_ok (Node (Black, a, y, b)) in
          d
      | Leaf ->
          (* guaranteed to be nonempty *)
          failwith "RBT insert failed with ins returning leaf"

    let remove k d = raise (Failure "Unimplemented: TreeDictionary.Make.remove")

    let rec find k d =
      let d = rep_ok d in
      match d with
      | Leaf -> None
      | Node (_, l, (k', v), r) ->
          if Key.compare k k' = LT then find k l
          else if Key.compare k k' = GT then find k r
          else Some v

    let rec member k d =
      let d = rep_ok d in
      match d with
      | Leaf -> false
      | Node (_, l, (k', _), r) ->
          if Key.compare k k' = LT then member k l
          else if Key.compare k k' = GT then member k r
          else true

    let to_list d =
      let d = rep_ok d in
      let rec inorder acc = function
        | Leaf -> acc
        | Node (_, l, y, r) -> inorder (y :: inorder acc r) l
      in
      inorder [] d

    let fold f (acc : 'acc) d =
      let d = rep_ok d in
      let rec inorder acc = function
        | Leaf -> acc
        | Node (_, l, (k, v), r) -> inorder (f k v (inorder acc l)) r
      in
      inorder acc d

    let to_string d =
      let d = rep_ok d in
      let rec inorder acc = function
        | Leaf -> acc
        | Node (c, l, (k, v), r) ->
            inorder
              (Key.to_string k ^ " maps to " ^ Value.to_string v
             ^ " which color is "
              ^ (if c = Red then "red" else "black")
              ^ "; " ^ inorder acc r)
              l
      in
      inorder "" d
  end
