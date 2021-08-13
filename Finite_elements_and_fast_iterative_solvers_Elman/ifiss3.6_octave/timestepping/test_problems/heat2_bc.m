function bc = specific_bc(xbd,ybd)
%heat2_bc   Reference problem T-1.2  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   IFISS function: DJS; 19 November 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
bc=zeros(size(xbd));
k=find(xbd==-1); bc(k)=1;
k=find(ybd==0);  bc(k)=1;
return