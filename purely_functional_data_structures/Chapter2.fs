module Chapter2

module Lists =
  type ListStack<'a> =
    'a list
  let Empty:ListStack<_> = []
  let isEmpty s = List.isEmpty s
  let cons a s:ListStack<_> = a :: s
  let hd s = List.head s
  let tl s = List.tail s

  type CustomStack<'a> =
    | Empty
    | Cons of 'a * CustomStack<'a>
  with
    static member empty = Empty
    // Note:
    // I tried naming this IsEmpty but it won't compile
    // F# auto generates Is* functions for each element of a DU.
    static member CheckEmpty = function
                           | Empty -> true
                           | _ -> false
    
    static member cons (x, s) =
      Cons(x, s)

    static member head = function
      | Empty -> failwith "empty"
      | Cons(x, _) -> x

    static member tail s =
      match s with
      | Empty -> failwith "empty"
      | Cons(x, Empty) -> x
      | Cons(x, s) -> CustomStack<'a>.tail s
  
  // I'm not a huge fan of how using `member` is shaping the code
  // above.  Here's another attempt where I use `module` to group
  // the related functions together and use `let` instead of member.
  module CustomStack =
    type CustomStack<'a> = Empty | Cons of 'a * CustomStack<'a>
    let empty = Empty
    
    let isEmpty = function Empty -> true | _ -> false
    
    let cons (x,s) = Cons(x, s)
    
    let head = function
      | Empty -> failwith "empty"
      | Cons(x,_) -> x

    let tail = function
      | Empty -> failwith "empty"
      | Cons(_, s) -> s

    let rec (++) a b =
      match a with
      | Empty -> b
      | Cons(x, s) -> Cons(x, s ++ b)

    let rec update = function
      | (Empty, _, _) -> failwith "subscript is invalid"
      | (Cons(_, s), 0, x) -> Cons(x, s)
      | (Cons(_, s), i, x) when i > 0 -> update (s, i-1, x)
      | _ -> failwith "subscript is invalid"  // There's a bug in the book which allows a negative subscript
    
    // Problem 2.1
    let rec suffixes =
      function
      | Empty -> Empty
      | s -> Cons(s, tail s |> suffixes)

module BinarySearchTrees =
  type Tree<'a> = Empty | Tree of Tree<'a> * 'a * Tree<'a>

  (*
    One thing to note is the disconnect between Tree<'a>
    and the isMember/insert functions.  The Type has no
    restrictions on a' but isMember/insert have a comparison
    restriction.

    ML has the functor concept to apply restructions on the
    type parameter for Tree.  In F#, I can probably make isMember
    and insert member methods on Tree.  OR add a restriction to
    'a in the definition of Tree.

    This is a good place to think about how we structure our code.
    The question is: do we add restrictions to Tree or not? To
    answer that, we need to ask ourselves some other questions
    - is Tree tightly coupled with the functions or is Tree a type
    and isMember/insert are basically just some functions that were
    written to use Tree.  In other words, are the functions intrinsic
    to how Tree will be used.
    - another Q?

    Following that, we can also ask ourselves is 'Tree' a good name
    given how we intend it to function and be used?
     - if the sorted insertion is inherent to Tree then the naming
     should reflect that functionality.  In someway, maybe name the
     module 'BinarySearchTree'

    Is our structure grouping with module sensible given how we expect
    Tree and the functions to be coupled?
      - If the functions are built on top of tree they shouldn't be in
      the same module
      - If the functions and Tree are tightly coupled then they should
      be in the same module.
  *)
  let rec isMember = function
    | (x,Empty) -> false
    | (x, Tree(l, n, r)) ->
      if x < n then isMember (x, l)   // Experimenting with a new format for if/then.  I like having the condition and it's direct conclusion on the same line
      elif x > n then isMember(x,r)
      else true

  let rec insert (x,s) =
    match s with
    | Empty -> Tree (Empty, x, Empty)
    | Tree(l,n,r) ->
      if x < n then Tree(insert (x,l), n, r)
      elif x > n then Tree(l, n, insert (x,r))
      else s