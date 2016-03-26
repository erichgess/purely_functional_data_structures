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

  let insert x h = merge (Heap(1, x, Empty, Empty)) h

  let findMin = function
    | Empty -> failwith "Empty Heap"
    | Heap(_, x, a, b) -> x

  let deleteMin = function
    | Empty -> failwith "Empty Heap"
    | Heap(_, x, a, b) -> merge a b