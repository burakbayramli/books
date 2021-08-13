function bc = specific_bc(xbd,ybd)
%quad_bc   channel flow boundary condition for 4:1 quadrilateral domain 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with Poiseuille flow
%   needs editing for general aspect ratios
%   IFISS function: DJS; 6 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
aspect=4; deform=4;
bc=0*xbd; 
k=find(xbd==0); bc(k)=2*ybd(k).*ybd(k) -(4/3)*ybd(k).*ybd(k).*ybd(k);
slope=(1/aspect)*(1/deform-1);
k3=find(ybd==slope*xbd +1);  bc(k3)=2/3;
return