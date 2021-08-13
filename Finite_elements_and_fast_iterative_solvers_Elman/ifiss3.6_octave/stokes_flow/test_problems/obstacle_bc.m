function bc = specific_bc(xbd,ybd)
%obstacle_bc   Obstacle problem stream function boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with obstacle flow
%   IFISS function: DJS; 1 December 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
bc=0*xbd;
k=find(xbd==0); bc(k)=ybd(k)-ybd(k).^3/3;
k=find(ybd==1); bc(k)=2/3;
k=find(ybd==-1); bc(k)=-2/3;
return