function bc = specific_bc(xbd,ybd)
%lateral_bc   Reference problem B1.1  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   IFISS function: DJS; 14 December 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
global H L 
bc=zeros(size(xbd));
k=find(xbd==0);  bc(k)=0.5;
k=find(xbd==L);  bc(k)=-0.5;
return