## Leftist Heaps
Heaps are a data structure used to pull out an extrema from a set of values in _O_(1)
time.  This could be the minimum value in the set or the maximum value in the set,
depending on the conditional operator used.  The implementation in _Purely Functional
Data Structures_ is for getting the minimum value.

The _Leftist Heap_ is a heap implementation that maintains the property: the rank of
any left child is at least as large as the rank of its right sibling.  The rank of a
node is the length of its right spline, which is the length of the right most path from
the node to an empty node.

Converting the implementation from the book into F# is pretty trivial in this case. One
thing that is interesting is that this book uses a tuple for all the function parameter
lists. However, idiomatic F# seems to be to use curried* functions (`let f x y = ...`).   << * is curried the right word here?
In previous posts, I've stuck to using the format from the book.  This chapter had the
first time where I felt that it was more convenient to use idiomatic F#.  As a result,
I've concluded that I should be converting into idiomatic F# as much as possible.  Now
I will use curried style functions unless using a tuple would make cleaner more readable
code.

// have sample showing tuple use.

Beyond tuples vs curried functions, there wasn't a whole lot that made me think about
F# coding style or implementation.  The data structure itself is very simple and fits
within F# very easily.