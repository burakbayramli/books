function bc = specific_bc(xbd,ybd)
%heat1_bc   Reference problem T-1.1  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   IFISS function: DJS; 19 November 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
bc=zeros(size(xbd));
k=find(xbd==0);  bc(k)=0.5;
k=find(xbd==1);  bc(k)=-0.5;
return