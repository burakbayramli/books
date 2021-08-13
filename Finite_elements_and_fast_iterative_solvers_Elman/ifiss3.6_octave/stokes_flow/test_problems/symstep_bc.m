function bc = specific_bc(xbd,ybd)
%symstep_bc   symmetric channel boundary condition
%   bc = specific_bc(xbd,ybd);
%   input
%          xbd          x boundary coordinate vector
%          ybd          y boundary coordinate vector 
%
%   specifies streamfunction associated with symmetric step flow
%   IFISS function: DJS; 19 November 2014.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage
bc=0*xbd;
k=find(xbd==-1); bc(k)=ybd(k).*(1-4*ybd(k).*ybd(k)/3);
k=find(ybd==-0.5); bc(k)=-1/3; k=find(ybd==-1); bc(k)=-1/3;
k=find(xbd==0 & ybd<-0.5);  bc(k)=-1/3;
k=find(ybd==0.5); bc(k)=1/3;  k=find(ybd==1);  bc(k)=1/3;
k=find(xbd==0 & ybd>0.5);  bc(k)=1/3; 
return