Boundary conditions:

v=0 on all sides. u=0 on right,left and bottom and u=lid velocity at top BC

Numerical method:

SIMPLE algorithm used for resolving velocity-pressure coupling. Staggered grid for u and v. To see how indexing works in staggered grid, Please check out "Versteeg, Malalasekera: an introduction to computational fluid dynamics" text book. Discretization of the governing equations is based on this text book. u and v are updated by Jacobi method in every iteration. Pressure correction equation is directly solved using a penta-diagonal matrix algorithm in every iteration. Proper choice of under-relaxation factors needed for convergence. Jacobi method is the least efficient way for this type of problems but it's simple and easy to prallelize. You are encouraged to apply other iterative methods such as line by line TDMA, Guess-seidel, multigrid or SOR for faster convergence.

Pressure correction:

I often see people struggling with solving the pressure correction equation in SIMPLE algorithm because the BCs for P' are unclear. here is how to tackle it:

1) pressure is a relative concept. So, clamp the pressure of one node in the domain to zero as a boundary condition and the pressure at other nodes will be measured relative to that point. It's a common practice to set P(1,1)=0 and because P(1,1) is known P'(1,1)=0, but you'll need to compute P' at other nodes. It's obvious that aS = 0 for a P' point that is located on the bottom wall because there is no P' node under it but it has aW,aE,aN. Other boundary P' values will be set this way. you'll end up with a penta-diagonal matrix of coefficients.

I have written this code in Parallel using PETSc (C language). I am going to upload it on GitHub pretty soon. It's much faster than this version because:

1) It is parallel and not serial so you can you can use multiple processors.

2) The C version solves the Momentum equations by iterating a couple of times unlike the MATLAB version which does only one Jacobi sweep.

3) Most importantly, PETSc uses Krylov subspace iterative method (type:GMRES) and it also uses preconditioners (I found Additive Shwartz PC the best) to solve momentum and pressure correction, which is way more efficient than Jacobi method in terms of convergence rate.

Cite As

MJ Sarfi (2021). 2D Lid Driven Cavity Flow using SIMPLE algorithm
(https://www.mathworks.com/matlabcentral/fileexchange/68348-2d-lid-driven-cavity-flow-using-simple-algorithm),
MATLAB Central File Exchange. Retrieved April 6, 2021.

Comments and Ratings (13)

Rate this submission
 
12345
 (Rating not required) 
Comment on this submission
Mark
2 Dec 2020

@atafiroozi - The solution will converge when the relaxation parameter for pressure is decreased. This value must be decreased further when the mesh is refined.
@sanjana malaimagal - Based on Finite volume method

sanjana malaimagal
5 Aug 2020

Can someone say, Is the discretization is based on the Finite difference or Finite Volume method

Aggelos Nikolaos Papakalodoukas
4 Jun 2020

Hello, I think that relaxation is can't be used at velocity field you can use it only for p*

Deep Morzaria
28 Mar 2020

Can someone tell me why the velocities in the momentum equations are evaluated explicitly ? Shouldn't they be calculated using neighbouring velocities at the same time step ?

michio
10 Mar 2020

Mingming Zhang
17 Nov 2019

@domenico bianchi Hi, he/she wrote MATLAB code in C/C++ preference, so column-first and row-first sometimes makes people puzzled :)

chuanxi wang
21 Oct 2019

GREAT！

Mohammad
14 Oct 2019

Robin
12 May 2019

Great work!
For a future update, each iteration will be about 75% faster when you initiate Ap as sparse(N,N), instead of zeros(N,N).

domenico bianchi
27 Apr 2019

in the matrix of coefficient file the variable position i think should be evaluated using the variable stride=imax, instead of stride=jmax as you wrote. In your code imax=jmax by chance but obviously if there is the need to change you have to use the correct value for stride. can you check, please?

atafiroozi
4 Feb 2019

This code doesn't converge when Re=1000, and I'm not sure why.
I would appreciate any help.

Yerkanat aydarken
18 Nov 2018

jn ch
1 Oct 2018

