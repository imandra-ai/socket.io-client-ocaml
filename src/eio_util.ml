let const : 'a -> 'b -> 'a =
  fun x _ -> x

let flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c =
  fun f a b -> f b a

module Char = struct
  let is_digit = function
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
    | _ -> false
end

module List = struct
  let split_at : int -> 'a list -> 'a list * 'a list =
    fun index list ->
      if index <= 0 then
        ([], list)
      else
        let rec loop i t accum =
          if i = 0 then
            (List.rev accum, t)
          else
            match t with
            | [] -> (list, [])
            | hd :: tl -> loop (i - 1) tl (hd :: accum)
        in
        loop index list []
end

module Lwt = struct
  let ignore_exn : (unit -> unit Lwt.t) -> unit Lwt.t =
    fun t ->
      Lwt.catch t
        (fun exn -> Lwt.return_unit)
end

module Option = struct
  let map : f:('a -> 'b) -> 'a option -> 'b option =
    fun ~f ->
      function
      | Some x -> Some (f x)
      | None -> None

  let iter : f:('a -> unit) -> 'a option -> unit =
    fun ~f ->
      function
      | Some x -> f x
      | None -> ()

  let value : default:'a -> 'a option -> 'a =
    fun ~default ->
      function
      | Some x -> x
      | None -> default

  let value_map : f:('a -> 'b) -> default:'b -> 'a option -> 'b =
    fun ~f ~default t ->
      map ~f t |> value ~default

  let to_list : 'a option -> 'a list =
    function
    | Some x -> [x]
    | None -> []
end

module String = struct
  let uncons : string -> (char * string) option =
    fun string ->
      let len = String.length string in
      if len = 0 then
        None
      else
        Some (String.get string 0, String.sub string 1 (len - 1))

  let split_at : int -> string -> (string * string) option =
    fun i string ->
      let len = String.length string in
      if i > len then
        None
      else
        Some (String.sub string 0 i, String.sub string i (len - i))
end
