function [bound,bc] = drivencavity_bc(xy)
%drivencavity_bc   driven cavity boundary condition for [-1,1]
%   [bound,bc] = drivencavity_bc(xy);
%   input
%          xy          nodal coordinate vector
%  output
%          bound        boundary dof vector
%          bc           boundary value
%   IFISS function: DJS; 3 December 2020.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
x=xy(:,1); y=xy(:,2); nn=length(x);
bc=zeros(4*nn,1);
bound=[];
%
% bottom edge
k1=find(y==-1); bc(k1)=0.0; % value
k2=k1+2*nn; bc(k2)=0.0;     % y-derivative
bound=[bound;k1;k2];
% top edge
k1=find(y==1);  bc(k1)=0.0; % value
k2=k1+2*nn; bc(k2)=1.0;     % y-derivative
bound=[bound;k1;k2];
% left edge
k1=find(x==-1);  bc(k1)=0.0; % value
k2=k1+nn; bc(k2)=0.0;        % x-derivative
bound=[bound;k1;k2];
% right edge
k1=find(x==1);  bc(k1)=0.0; % value
k2=k1+nn; bc(k2)=0.0;       % x-derivative
bound=[bound;k1;k2];
%
bound=unique(bound);  % remove redundancies
bc=bc(bound);
return
