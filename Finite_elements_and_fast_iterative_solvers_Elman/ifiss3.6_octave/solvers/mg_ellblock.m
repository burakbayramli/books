function [P1,P2]=mg_ellblock(nelx,nely,x,y)        
%MG_ELLBLOCK prolongation for part of L-shaped or step domain
%   [P1,P2] = mg_ellblock(nelx,nely,x,y);
%   input
%          nelx    number of elements in x-direction
%          nely    number of elements in y-direction
%          x       x coordinate vector for coarse grid
%          y       y coordinate vector for coarse grid
%   output
%          P1      component of prolongation operator for upper left of
%                  step [-1,h]x[0,1]
%          P2      component of prolngation operator for border between
%                  upper left of step and right part of step
%
%   IFISS function: AR; 19 November, 2001.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage
ecx=nelx/2; ecy=nely/2;
tx = x(2:nelx+1) - x(1:nelx);
bx = zeros(nelx,1);
bx(1:2:nelx-1) = x(3:2:nelx+1) - x(1:2:nelx-1);
bx(2:2:nelx)   = bx(1:2:nelx-1);
dx = tx./bx;
%
ty = y(2:nely+1) - y(1:nely);
by = zeros(nely,1);
by(1:2:nely-1) = y(3:2:nely+1) - y(1:2:nely-1);
by(2:2:nely)   = by(1:2:nely-1);
dy = ty./by;
%
p = spalloc(2*ecx+1,ecx+1,3*ecx+1);
for j=2:ecx
   p(2*j-2,j) = dx(2*j-3);
   p(2*j-1,j) = 1.0;
   p(2*j  ,j) = dx(2*j);
end
j=1;
   p(2*j-1,1) = 1.0;           
   p(2*j  ,1) = dx(2*j);
j=ecx+1;
   p(2*j-2,j) = dx(2*j-3);
   p(2*j-1,j) = 1.0;
%
% main block (last row/column omitted)
P1=spalloc( (2*ecy+1)*(2*ecx), (ecy+1)*(ecx), (3*ecy+1)*(3*ecx+1) );
%
sp=p(1:end-1,1:end-1);
for j=2:ecy,
    cols = (j-1)*(ecx)+[1:ecx];
    P1((2*j-3)*(2*ecx)+[1:2*ecx],cols) = dy(2*j-3)*sp;
    P1((2*j-2)*(2*ecx)+[1:2*ecx],cols) = sp;
    P1((2*j-1)*(2*ecx)+[1:2*ecx],cols) = dy(2*j)*sp;
end
j=1;
    cols = (j-1)*(ecx)+[1:ecx];
    P1((2*j-2)*(2*ecx)+[1:2*ecx],cols) = sp;
    P1((2*j-1)*(2*ecx)+[1:2*ecx],cols) = dy(2*j)*sp;
j=ecy+1;
    cols = (j-1)*(ecx)+[1:ecx];
    P1((2*j-3)*(2*ecx)+[1:2*ecx],cols) = dy(2*j-3)*sp;
    P1((2*j-2)*(2*ecx)+[1:2*ecx],cols) = sp;
%
% coupling with large block
P2=spalloc((2*ecy+1)*(2*ecx),(ecy+1)*(ecx+1),(3*ecy+1)*(3*ecx+1) );
entry=p(2*ecx,ecx+1);
for j=2:ecy,
    cols = (j-1)*(ecx+1)+1;
    P2((2*j-3)*(2*ecx)+[2*ecx],cols) = dy(2*j-3)*entry;
    P2((2*j-2)*(2*ecx)+[2*ecx],cols) = entry;
    P2((2*j-1)*(2*ecx)+[2*ecx],cols) = dy(2*j)*entry;
end
j=1;
    cols = (j-1)*(ecx+1)+1;
    P2((2*j-2)*(2*ecx)+[2*ecx],cols) = entry;
    P2((2*j-1)*(2*ecx)+[2*ecx],cols) = dy(2*j)*entry;
j=ecy+1;
    cols = (j-1)*(ecx+1)+1;
    P2((2*j-3)*(2*ecx)+[2*ecx],cols) = dy(2*j-3)*entry;
    P2((2*j-2)*(2*ecx)+[2*ecx],cols) = entry;
return
