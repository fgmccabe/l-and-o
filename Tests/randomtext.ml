type ltable = (string * string list) list
type distribution =
  { total : int ;
    amounts : (string * int) list }
type htable = (string, distribution) Hashtbl.t
type ptable =
  { prefix_length : int ;
    table : (string list, distribution) Hashtbl.t }

let simple_0 =
  "I am a man and my dog is a good dog and a good dog makes a good man"

let simple_1 =
  "a good dad is proud of his son and a good son is proud of his dad"

let simple_2 =
  "a good woman is proud of her daughter and a good daughter is proud of her mom"

let simple_3 =
  "there is a beer in a fridge in a kitchen in a house in a land where \
   there is a man who has a house where there is no beer in the kitchen"

let multi_1 =
  "A good dad is proud of his son. \
   A good son is proud of his dad."

let multi_2 =
  "A good woman is proud of her daughter. \
   A good daughter is proud of her mom."

let multi_3 =
  "In a land of myths, and a time of magic, \
   the destiny of a great kingdom rests \
   on the shoulders of a young man."

(* -- Part A -------------------------------------------------------------- *) 

let words str =
  let len = String.length str in
  let isLetter ch = (ch>='A' && ch<='Z') || (ch>='a' && ch<='z') || (ch>='0' && ch<='9') in 
  let rec
    stream ix soFar = 
    if ix>=len then
      soFar
    else
      let ch = str.[ix] in
      if isLetter ch then
        let buf = Buffer.create 8 in
        nextWord ix buf soFar
      else
        stream (ix+1) soFar and
    addWord word soFar = List.append soFar [word] and
    nextWord ix buf soFar =
                                                    if ix>=len then
                                                      addWord (Buffer.contents buf) soFar
                                                    else
                                                      let ch = str.[ix] in
                                                      if isLetter ch then
                                                        let _ = Buffer.add_char buf ch in
                                                        nextWord (ix+1) buf soFar
                                                      else
                                                        stream (ix+1) (addWord (Buffer.contents buf) soFar)
  in
  stream 0 [];;
             
let addWord tbl fr nxt = 
  let rec add tbl = match tbl with 
    | [] -> [(fr,[nxt])]
    | (ky,w)::l when ky = fr -> (ky,(List.append w [nxt]))::l
    | p::l -> p::(add l) in 
  add tbl;;

let build_ltable words = 
  let rec build curr soFar = function
    | [] -> addWord soFar curr "STOP"
    | w::r -> build w (addWord soFar curr w) r in
  build "START" [] words;;

let next_in_ltable table word =
  let sub = List.assoc word table in
  let nth = Random.int (List.length sub) in
  List.nth sub nth;;

let walk_ltable table =
  let rec build curr soFar = 
    let nxt = next_in_ltable table curr in
    if nxt = "STOP" then
      soFar
    else
      build nxt (List.append soFar [nxt])
  in build "START" [];;

(* -- Part B -------------------------------------------------------------- *)

let compute_distribution l =
  let sorted = List.sort compare l in
  let rec build curr cx l soFar = match l with
    | [] -> (curr,cx)::soFar
    | w::l when w=curr -> build curr (cx+1) l soFar
    | w::l -> build w 1 l ((curr,cx)::soFar)
  in {total = List.length l;amounts=(match sorted with 
      | [] -> []
      | w::l -> build w 1 l [])};;

let build_htable words =
  let tbl = Hashtbl.create (List.length words) in
  let addWord w nxt =
    if Hashtbl.mem tbl w then
      let curr = Hashtbl.find tbl w in
      Hashtbl.replace tbl w (nxt::curr)
    else
      Hashtbl.replace tbl w [nxt] in
  let rec build curr = function
    | [] -> addWord curr "STOP"
    | w::l -> let () = addWord curr w in
        build w l in 
  let () = build "START" words in
  let final = Hashtbl.create (List.length words) in
  let summarize word suffixes = 
    Hashtbl.add final word (compute_distribution suffixes) in
  let () = Hashtbl.iter summarize tbl in
  final;;

let next_in_htable table word =
  let dist = Hashtbl.find table word in
  let nth = Random.int (dist.total) in
  let rec pick ix = function
    | [] -> raise Not_found
    | (nxt,amnt) :: _ when amnt>ix -> nxt
    | (_,amnt)::l -> pick (ix-amnt) l in
  pick nth dist.amounts;; 

let walk_htable table = 
  let rec build curr soFar = 
    let nxt = next_in_htable table curr in
    if nxt = "STOP" then
      soFar
    else
      build nxt (List.append soFar [nxt])
  in build "START" [];; 

(* -- Part C -------------------------------------------------------------- *)
exception Break;;

let sentences str =
  let len = String.length str in
  let isLetter ch = (ch>='A' && ch<='Z') || 
                    (ch>='a' && ch<='z') || 
                    (ch>='0' && ch<='9') || 
                    (ch>='\128' && ch<='\255') in 
  let isPunc ch = List.mem ch [';'; ','; ':'; '-'; '"'; '\''; '?'; '!'; '.'] in
  let isTerm w = List.mem w ["?"; "!";"."] in
  let isSep ch = not (isLetter ch) && not (isPunc ch) in
  let ix = ref 0 in
  
  let parseWord () =
    let wBuff = Buffer.create 8 in (
      if !ix<len && isPunc str.[!ix] then (
        Buffer.add_char wBuff str.[!ix] ;
        ix := !ix+1
      ) else while !ix<len && isLetter str.[!ix] do
          Buffer.add_char wBuff str.[!ix];
          ix := !ix+1
        done;
      (Buffer.contents wBuff)
    ) in
  
  let skipSeps () =
    while !ix<len && isSep str.[!ix] do
      ix := !ix+1
    done in
  
  let parseSentence () =
    let words = ref [] in
    try
      while !ix<len do
        skipSeps ();
        let word = parseWord() in (
          if (String.length word)>0 then
            words := word::!words; 
          
          if isTerm word then
            raise Break
        )
      done;
      List.rev (!words)
    with
    | Break ->
        List.rev (!words) in
  
  let isEmptySentence = function
    | [] -> true
              (* | [e] -> isTerm e *)
    | _ -> false in
  
  let parseSentences () = 
    let sentences = ref [] in
    
    ( while !ix<len do
        let nxt = parseSentence() in
        if not (isEmptySentence nxt) then
          sentences := nxt::!sentences
      done;
      List.rev !sentences) in
    
  parseSentences() ;;
    
let rec start ix =
  if ix = 0 then [] else "START"::(start (ix-1));; 

let shift l x = let
  shift = function 
    | [] -> [x]
    | _::l -> List.append l [x]
  in shift l;;

let rec split cx l =
  if cx = 0 || l=[] then ([],l) else
    let (p,r) = split (cx-1) (List.tl l) in
    ((List.hd l)::p,r);; 

let build_ptable words pl = 
  let tbl = Hashtbl.create (List.length words) in
  let addWords ws nxt =
    if Hashtbl.mem tbl ws then
      let curr = Hashtbl.find tbl ws in
      Hashtbl.replace tbl ws (nxt::curr)
    else
      Hashtbl.replace tbl ws [nxt] in
  let rec build ws =
    if (List.length ws) < pl then
      addWords ws "STOP"
    else
      let (prefix, rest) = split pl ws in
      match rest with
      | [] -> addWords ws "STOP" (*should never happen *)
      | first::l -> 
          addWords prefix first;
          build (List.tl ws) in 
  let () = build (List.append (start pl) words) in
  let final = Hashtbl.create (List.length words) in
  let summarize word suffixes = 
    Hashtbl.add final word (compute_distribution suffixes) in
  let () = Hashtbl.iter summarize tbl in
  { prefix_length=pl; table=final};;

let walk_ptable { table ; prefix_length = pl } = 
  let rec build curr soFar = 
    let nxt = next_in_htable table curr in
    if nxt = "STOP" then
      soFar
    else
      build (shift curr nxt) (List.append soFar [nxt])
  in build (start pl) [];; 

let mergeDistributions = fun 
  {total=tot1; amounts=amnts1} {total=tot2;amounts=amnts2} -> 
  let incCount = fun tbl (str,cnt) ->
    let rec check = function
      | [] -> [(str,cnt)]
      | (s,c)::l when s=str -> (s,c+cnt)::l
      | e::l -> e::(check l) in
    check tbl in
  {total=tot1+tot2;amounts=List.fold_left incCount amnts1 amnts2};;
  
let merge_ptables = fun 
  ({prefix_length=pl;table=tbl}::tbls) ->
  let tbl' = Hashtbl.copy tbl in
  let mergeEntry pfx dist =
    if Hashtbl.mem tbl' pfx then
      Hashtbl.replace tbl' pfx (mergeDistributions (Hashtbl.find tbl' pfx) dist)
    else
      Hashtbl.add tbl' pfx dist in
  let addInTble = 
    fun {prefix_length=p;table=t} ->
      Hashtbl.iter mergeEntry t in
  (List.iter addInTble tbls;
   {prefix_length=pl;table=tbl'});;
  

