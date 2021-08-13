function bc = specific_bc(xbd,ybd)
%heat3_bc   Reference problem T-1.3  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   IFISS function: DJS; 12 November 2011.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
bc=ones(size(xbd))*(0.5);
k=find(ybd==1);  bc(k)=-0.5;
return