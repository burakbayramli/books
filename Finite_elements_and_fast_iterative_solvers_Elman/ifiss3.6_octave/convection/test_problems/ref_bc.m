function bc = specific_bc(xbd,ybd)
%ref_bc   Reference problem 3.2  boundary condition 
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%   definition of Neil Madden test problem given in Roos book p.197.
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
nobd=length(xbd);
bc=zeros(size(xbd));
k=find(ybd==-1); bc(k)=1;
k=find( xbd ==-1); bc(k)=(1-(1+ybd(k))/2).^3;
k=find( xbd ==1 ); bc(k)=(1-(1+ybd(k))/2).^2;
return