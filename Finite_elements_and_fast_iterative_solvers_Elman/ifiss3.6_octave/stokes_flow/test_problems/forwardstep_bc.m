function bc = specific_bc(xbd,ybd)
%forwardstep_bc  Forward facing step flow boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with Forward step flow
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bc=0*xbd;
k=find(xbd==5); bc(k)=(ybd(k).^3 -3*ybd(k) -2)/6;
k=find(ybd==1); bc(k)=-2/3;
return