
// Another meaningless .cnc file just to test handling of the various constructions.
//================================================================================

// Tag functions can be bounded.
// items and tags can be dense.
// Dense & bounded is not a GUARANTEE of total coverage in that interval.


//tags <(int,int)>            T with dense = true;
dense tags <(int,int)>  T;

items<(int,int), Foo>   I; 
steps                   S;

/*

props(T): [("dense", true)]

Or could just move this whole thing over to JSON or something.

{ 'name' : 'T', 'contains' : 'tags', 'dense'  : true }

 */


// ================================================================================

/*
bounded I 0:N, 0:2*M;
bounds I[i,j]: i in [0:N), j in [0:M);

bounded 0:N, 0:2*M I;
 
I[i,j] bounds: i >= 0, i < N, j >= 0, j < M;

range I[i,j]: i >= 0, i < N, j >= 0, j < M;

constrain I[i,j]:  i >= 0,  i < N,   j >= 0,  j < M;

param N;
I == [1..N]
  What is the most popular range notation?

*/

//constrain I[i,j] where i>=0,  i<N,  j>=0,  j<M;

// Here I am playing with some possibilities for how to bound a space:

constrain I[i,j]  i>=0,  i<N,  j>=0,  j<M;
//constrain I[i,j] i <= 3, j==2, i < N;
//constrain I;
//constrain ;

env -> T;
env <- I;
T prescribes S; 

// Here is a wavefront style computation.
I[i-1, j], 
I[i, j-1], 
I[i-1, j-1] -> S(i,j) -> I[i,j];

// BUT that tag function is BOUNDED by the rectangular bounds of I[] above.

// Is this enough to compute the boundary conditions for and getcounts?
// And to compute it symbolically without specifying N and M?

// And do we want to put the bounds on the item collection itself?  Or on that particular tag function?

