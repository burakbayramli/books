function bc = specific_bc(xbd,ybd)
%bottom_bc   Reference problem B1.2  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   IFISS function: DJS; 14 December 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
global H L 
bc=zeros(size(xbd));
k=find(ybd==0);  bc(k)=0.5;
k=find(ybd==H);  bc(k)=-0.5;
return