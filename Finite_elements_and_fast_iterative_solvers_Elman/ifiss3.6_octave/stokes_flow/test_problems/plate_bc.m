function bc = specific_bc(xbd,ybd)
%plate_bc   Reference problem 7.4 boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with Blasius flow
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
bc=0*xbd;
k=find(xbd==-1); bc(k)=ybd(k);
k=find(ybd==1); bc(k)=1;
k=find(ybd==-1); bc(k)=-1;
return