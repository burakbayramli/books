function neumannb=out_Neumann_bound(mv,xy)
%OUT_NEUMANN_BOUND locates elements on the step outflow boundary
% neumannb=out_Neumann_bound(mv,xy);
%   input
%          xy           vertex coordinate vector  
%          mv           element mapping matrix
%   output
%          neumannb     elements on the right hand side of the domain
%
%   IFISS function: QL; 19 May 2010.
% Copyright (c) 2005 D.J. Silvester, Qifeng Liao

xx=xy(:,1);
max_x=max(xx);
melx=xy(mv(:,2),1);
tol=10^(-10);
t=find(abs(melx-max_x)<tol);
nt=length(t);
neumannb=zeros(nt,2);
neumannb(:,1)=t;
neumannb(:,2)=2;
