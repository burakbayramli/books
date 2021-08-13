function bc = specific_bc(xbd,ybd)
%test_bc   Reference problem 3.3  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nobd=length(xbd);
bc=zeros(size(xbd));
right=find(xbd==1 & ybd <= 1);
bottom=find(ybd==-1 & xbd >= 0);
bc(right)=1;bc(bottom)=1; 
return