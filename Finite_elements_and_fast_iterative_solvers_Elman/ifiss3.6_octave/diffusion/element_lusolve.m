function xx = element_lusolve(ae,fe)
%ELEMENT_LUSOLVE vectorized local backward-forward solves      
%  xx = element_lusolve(ae,fe);
%   input
%          ae    LDLT factorised element matrices  
%          fe    elementwise RHS vectors
%   output
%          xx    elementwise solution vectors
%   IFISS function: DJS; 2 January 2011 
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 
[nel,nn,nnx]=size(ae);
% unpack diagonal
for kk=1:nn
    dd(1:nel,kk)=ae(1:nel,kk,kk);
end
% forward substitution
yy=zeros(nel,nn); zz=zeros(nel,nn); xx=zeros(nel,nn); 
yy(1:nel,1)=fe(1:nel,1);
for ii=2:nn;
    yy(1:nel,ii)=fe(1:nel,ii);
    for jj=1:ii-1,
        yy(1:nel,ii)=yy(1:nel,ii)-ae(1:nel,ii,jj).*yy(1:nel,jj);
    end
end
% diagonal solve
for ii=1:nn
    zz(1:nel,ii)=yy(1:nel,ii)./dd(1:nel,ii);
end
% backward substitution
    xx(1:nel,nn)=zz(1:nel,nn);
for ii=nn-1:-1:1;
    xx(1:nel,ii)=zz(1:nel,ii);
    for jj=ii+1:nn,
        xx(1:nel,ii)=xx(1:nel,ii)-ae(1:nel,jj,ii).*xx(1:nel,jj);
    end
end
return
