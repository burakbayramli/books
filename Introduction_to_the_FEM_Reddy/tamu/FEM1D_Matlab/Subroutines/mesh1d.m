function [glx,nod]=mesh1d(nem,npe,mxelm,mxnod,dx)
% c     __________________________________________________________________
% c
% c     the subroutine is called in main to compute arrays {glx} and [nod]
% c
% c        {glx}.... vector of global coordinates
% c        {dx}..... vector of element lengths [dx(1) = node 1 coord.]
% c        [nod].... connectivity matrix
% c     __________________________________________________________________
% c
%   implicit real*8 (a-h,o-z)
%   dimension glx(mxnod),dx(mxnod),nod(mxelm,4)
%c  generate the elements of the connectivity matrix

for i=1:npe
    nod(1,i)=i;
end
for n=2:nem
    for i=1:npe
        nod(n,i) = nod(n-1,i)+npe-1;
    end
end
%c  generate global coordinates of the global nodes
glx(1)=dx(1);
if(npe==2)
    for i=1:nem
        glx(i+1) = glx(i) + dx(i+1);
    end
else
    for i=1:nem
        ii=2*i;
        glx(ii) = glx(ii-1) + 0.5*dx(i+1);
        glx(ii+1)=glx(ii-1) + dx(i+1);
    end
end
end  
