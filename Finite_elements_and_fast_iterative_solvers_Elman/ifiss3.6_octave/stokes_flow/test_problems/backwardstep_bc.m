function bc = specific_bc(xbd,ybd)
%backwardstep_bc   Reference problem 5.2 boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with Backward step flow
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bc=0*xbd;
k=find(xbd==-1); bc(k)=2*ybd(k).*ybd(k).*(1-2*ybd(k)/3);
k=find(ybd==1); bc(k)=2/3;
return