(* Basic Streams *)
type 'x thunk = unit -> 'x;;
type 'x stream = Nil | Cons of 'x * ('x stream thunk);;

let streamHd (s : 'a stream) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons (x, _) -> x

let streamTl (s : 'a stream) : 'a stream =
  match s with
    Nil -> failwith "tl"
  | Cons (_, g) -> g ()

let from_list (l : 'a list) : 'a stream =
  List.fold_right (fun x s -> Cons (x, fun () -> s)) l Nil

let rec take (n : int) (s : 'a stream) : 'a list =
  if n <= 0 then [] else
  match s with
    Nil -> []
  | _ -> streamHd s :: take (n - 1) (streamTl s)

let rec map (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  match s with Nil -> Nil
  | _ -> Cons (f (streamHd s), fun () -> map f (streamTl s))

let rec repeat (x : 'a) : 'a stream = Cons(x, fun () -> repeat x)

let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
  match s with Nil -> Nil
  | Cons (x, g) ->
      if f x then Cons (x, fun () -> filter f (g ()))
      else filter f (g ())

let rec seq (strA : 'x stream) (strB : 'x stream): ('x stream) =
  match strA with
    | Cons (x, tl) -> Cons (x, fun () -> (seq (tl ()) strB))
    | Nil -> strB;;

let sub ~reduce ~substitute ~acc ~s =
  let rec inner acc s =
    match s with
    | Cons(x, thunk) ->
      let
        nextAcc = reduce acc x and substitution = substitute acc x
      in (
        match substitution with
        | Some stream -> stream
        | None -> Cons(x, fun () -> inner nextAcc (thunk ()))
      )
    | Nil -> Nil
  in
    inner acc s;;

(* Music/Timing *)

type 'x advancement = { dur: int; ev: 'x };;
type 'x embedding = { s: int; ev: 'x; };;

type ton = { note: int; };;

let genAdv dur ev = { dur; ev; }

let metro (note : int) (dur : int) =
  let rec inc (n : int): 'x advancement stream  =
    Cons ({ dur; ev = [ { note; } ] }, fun () -> inc (1 + (n * -1)) )
  in
    inc 1

let subOnTime = sub ~reduce: (fun t adv -> t + adv.dur) ~acc: 0;;

let limitDur limit = subOnTime ~substitute: (fun t adv ->
  if (t + adv.dur >= limit)
    then Some (Cons(genAdv (limit - t) adv.ev, fun () -> Nil))
    else None
);;

let rec limitDurVerbose (max : int) (advStr : 'x advancement stream) =
  match advStr with
    | Cons (hd, tl) ->
      if hd.dur >= max
        then Cons({ dur = max; ev = hd.ev }, fun () -> Nil)
        else Cons(hd, fun () -> limitDurVerbose (max - hd.dur) (tl ()))
    | Nil -> Nil;;

let tupleize tle = List.map (fun ton -> (tle.s, ton.note)) tle.ev;;

let embedAdvStr (advStr : 'a advancement stream): ('a embedding list) =
  let rec embed (offset : int) (acc : 'a embedding list) (advStr : 'a advancement stream) =
    match advStr with
      | Cons (adv, tl) ->
        embed (offset + adv.dur) ({ s = offset; ev = adv.ev; } :: acc) (tl ())
      | Nil -> List.rev acc
  in
    embed 0 [] advStr;;

let rest (dur : int): 'a list advancement = { dur = dur; ev = [] };;
let restStream (dur : int): 'a list advancement stream = from_list [(rest dur)];;

let rec par (lasA : 'a list advancement stream) (lasB : 'a list advancement stream) =
  match (lasA, lasB) with
    | (Cons(hdA, tlA), Cons(hdB, tlB)) ->
      let joined = genAdv hdA.dur (hdA.ev @ hdB.ev) in
        if hdA.dur = hdB.dur
          then Cons(
            joined,
            fun () -> par (tlA ()) (tlB ())
          )
          else
            if hdA.dur < hdB.dur
              then Cons(
                joined,
                fun () -> par (tlA ()) (seq (restStream (hdB.dur - hdA.dur)) (tlB ()))
              )
              else par lasB lasA
    | (a, Nil) -> a
    | (Nil, b) -> b;;

let rec erden (figure : ('a -> 'b) advancement stream) (ground : 'a advancement stream) : 'b advancement stream =
  match (figure, ground) with
    | (_, Nil) -> Nil
    | (Nil, _) -> Nil
    | (Cons(f, ft), Cons(g, gt)) -> Cons(
        genAdv f.dur (f.ev g.ev),
        fun () ->
          if f.dur = g.dur
          then erden (ft ()) (gt ())
          else
            if f.dur < g.dur
            then erden (ft ()) (Cons(genAdv (g.dur - f.dur) g.ev, gt))
            else erden (Cons(genAdv (f.dur - g.dur) f.ev, ft)) (gt ())
      );;

let artic3 =
  let rec spin p =
    Cons(genAdv 3 (fun n -> n + p), (fun () -> spin ((p + 1) mod 4)))
  in
    spin 0;;
