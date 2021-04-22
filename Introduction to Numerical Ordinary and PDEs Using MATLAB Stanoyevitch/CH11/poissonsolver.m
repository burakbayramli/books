function [xgrid, ygrid, Zsol] = poissonsolver(q,a,b,c,d,h)
%M-file for EFR 11.5.  This program is designed to find the finite
%difference solution of the Poisson problem with zero Dirichlet 
%boundary conditions on any rectangle R={a<=x<=b, c<=y<=d}:  
%u_xx + u_yy = q(x,y) on R
%u(x,y) =  0 on bdy R
%Input variables:  q = inline function (or M-file) for inhomogeneity
%a,b,c,d = endpoints of the rectangle R (a<b and c<d), and h =
%uniform step size (x step = y step).  It is assumed that h divides 
%into both b-a and d-c with an integer quotient.
%Output variables:  xgrid, ygrid, and Zsol,  the first two are vectors
%corresponding to the partion of [a, b] and [c, d] respectively determined
%by the step size h.  Zsol is the corresponding matrix of values over the 
%rectangular grid, set up so that surf(xgrid, ygrid, Zsol) will result in 
%a surface plot of the numerical solution.

%first check to see if h is a permissible step size:
if ((b-a)/h>floor((b-a)/h+eps))|((d-c)/h>floor((d-c)/h+eps))
    error('Inputted step size does not evenly divide into both side lengths; try another step size')
end

N=floor((b-a)/h)-1; %number of internal x-grid points
M=floor((d-c)/h)-1; %number of internal y-grid points
xgrid=linspace(a,b,N+2);, ygrid=linspace(c,d,M+2);

A=4*eye(N*M);
%form sub/super diagonals
a1=-ones(1,N-1);, a1rep = [0 a1];
for i=1:M-1, a1=[a1 a1rep];, end
aN=-1*ones(1,N*M-N);
%put these diagonal entries on A
A=A+diag(a1,-1)+ diag(a1,1)+diag(aN,-N)+diag(aN,N);
% First we construct a row vector for Q, arising from the source term:
% We do this by collecting the needed entries in the required reading order (using an 
% appropriately designed loop).
row = 1;
for j=M+1:-1:2
    count=(row-1)*N+1;
    Q(:,count:count+N-1)=q(xgrid(2:N+1),ygrid(j));, row = row+1;
end
C= -h^2*Q; ,  C=C';
%Now we are ready to solve the system.
U=A\C;
Z=zeros(N,M);
Z(:)=U;
Z=Z';
Z=[zeros(1,N); Z; zeros(1,N)];
Zsol=[zeros(1,M+2); Z'; zeros(1,M+2)]';
%rather than reverse the order of ygrid, we leave it in the usual order,
%but change the ordering in Zsol to make it amenable to 3D plotting.
%Znew = zeros(size(Zsol));
for i=1:M+2, Znew(i,:)=Zsol(M+3-i,:);, end, Zsol = Znew;


