module Chapter3

module LeftistHeaps =
  type Heap<'a> =
    |Empty
    |Heap of int * 'a * Heap<'a> * Heap<'a>

  let rank = function
    | Empty -> 0
    | Heap(r, _, _, _) -> r

  let makeT (x, a, b) =
    if rank a >= rank b
    then Heap(rank b + 1, x, a, b)
    else Heap(rank a + 1, x, b, a)

  let rec merge h1 h2 = 
    match (h1, h2) with
    | (h, Empty)
    | (Empty, h) -> h
    | (Heap(_, x, a1, b1), Heap(_, y, a2, b2)) -> 
      if x <= y
      then makeT(x, a1, merge b1 h2)
      else makeT(y, a2, merge h1 b2)

  /// I changed the ordering of the parameters here (from x,h to h, x)
  /// to facilitate using the fold function
  let insert h x = merge (Heap(1, x, Empty, Empty)) h

  let findMin = function
    | Empty -> failwith "Empty Heap"
    | Heap(_, x, a, b) -> x

  let deleteMin = function
    | Empty -> failwith "Empty Heap"
    | Heap(_, x, a, b) -> merge a b

  let fromList l =
    let rec mergeHeapList = function
      | [] -> []
      | a::[] -> [a]
      | a::b::t -> (merge a b) :: (mergeHeapList t)

    let rec execute = function
      | [] -> Empty
      | a::[] -> a
      | l -> execute (mergeHeapList l)

    let heapList = l |> List.map (insert Empty)
    execute heapList

module BinomialHeap =
  type Tree<'a> = Node of int * 'a * (Tree<'a> list)

  let root (Node(_, x, _)) = x
  
  let link t1 t2 =
    let (Node(r, x1, c1)) = t1
    let (Node(_, x2, c2)) = t2
    if x1 <= x2
    then Node(r + 1, x1, t2::c1)
    else Node(r+ 1, x2, t1::c2)

  type Heap<'a> = Tree<'a> list

  let rank (Node(r, _, _)) = r

  let rec insertTree t tl =
    (t,tl)
    |> function
    | (t, []) -> [t]
    | (t, t'::tl') -> 
      if rank t < rank t'
      then t::tl
      else insertTree (link t t') tl'

  let rec merge t1 t2 =
    match (t1, t2) with
    | (t1, []) -> t1
    | ([], t2) -> t2
    | (h1::t1', h2::t2') ->
      if rank h1 < rank h2
      then h1 :: (merge t1' t2)
      elif rank h2 < rank h1
      then h2 :: (merge t1 t2')
      else insertTree (link h1 h2) (merge t1' t2')

  let rec removeMinTree = function
    | [] -> failwith "Empty"
    | [t] -> (t, [])
    | t::ts ->
      let (t', ts') = removeMinTree ts
      if root t <= root t'
      then (t, ts)
      else (t', t::ts)

  let findMin t =
    removeMinTree t
    |> fst
    |> root

  // here's a point free version of the same function.
  let findMin'<'a> = removeMinTree >> fst >> root

  let deleteMin ts =
    let (Node(_, x, ts1), ts2) = removeMinTree ts
    merge (List.rev ts1) ts2