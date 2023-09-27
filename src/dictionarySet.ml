open Dictionary

module type ElementSig = sig
  type t

  include Dictionary.KeySig with type t := t
end

module type Set = sig
  module Elt : ElementSig

  type elt = Elt.t
  type t

  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list

  include Stringable with type t := t
end

module Make =
functor
  (E : ElementSig)
  (DM : DictionaryMaker)
  ->
  struct
    module Elt = E

    type elt = Elt.t

    (** Module Value is what we pass in as the argument of type ValueSig for the
        Dictionary Maker, since we don't care about what's stored as Values, it
        is justifiable to [@@coverage off] the function to_string here, which is
        something that I've discussed with one of the TAs. *)

    module Value = struct
      type t = unit

      let to_string (x : t) = ""
    end

    module SetDictionary = DM (Elt) (Value)

    type t = SetDictionary.t
    (** AF: The Dictionary with keys k1, ..., kn represents the Set with
        elements k1, ..., kn. RI: Would have already been covered by the
        representation invariants of the underlying dictionaries produced by DM *)

    let rep_ok (s : t) : t = s [@@coverage off]
    let empty = SetDictionary.empty
    let is_empty s = s = empty
    let size s = SetDictionary.size s
    let insert x s = SetDictionary.insert x () s
    let member x s = SetDictionary.member x s
    let remove x s = SetDictionary.remove x s

    let fold (f : elt -> 'acc -> 'acc) (init : 'acc) (s : t) =
      let f' k _ init = f k init in
      SetDictionary.fold f' init s

    let union s1 s2 = fold insert s2 s1

    let intersect s1 s2 =
      fold (fun x init -> if member x s2 then insert x init else init) empty s1

    let difference s1 s2 =
      fold
        (fun x init -> if not (member x s2) then insert x init else init)
        empty s1

    let to_list s =
      let x = SetDictionary.to_list s in
      List.map (fun (k, v) -> k) x

    let to_string s =
      s |> to_list
      |> List.fold_left (fun acc x -> acc ^ Elt.to_string x ^ " ") ""
  end
