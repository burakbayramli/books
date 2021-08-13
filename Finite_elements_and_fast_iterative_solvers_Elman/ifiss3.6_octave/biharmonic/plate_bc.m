function [bound,bc] = plate_bc(xy)
%plate_bc   modified clamped plate boundary condition for [-1,1]
%   [bound,bc] = plate_bc(xy);
%   input
%          xy          nodal coordinate vector
%  output
%          bound        boundary dof vector
%          bc           boundary value
%   IFISS function: DJS; 30 July 2021.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
x=xy(:,1); y=xy(:,2); nn=length(x);
bc=zeros(4*nn,1);
bound=[];
%
% bottom edge
k1=find(y==-1); bc(k1)=0.0; % value <------ scale domain vertically
k2=k1+nn;   bc(k2)=0.0;     % x-derivative
k3=k1+2*nn; bc(k3)=0.0;     % y-derivative
bound=[bound;k1;k2;k3];
% top edge
k1=find(y==1);  bc(k1)=0.0; % value <------ scale domain vertically
k2=k1+nn;   bc(k2)=0.0;     % x-derivative
k3=k1+2*nn; bc(k3)=0.0;     % y-derivative
bound=[bound;k1;k2;k3];
% left edge
k1=find(x==-1);  bc(k1)=0.0; % value
k2=k1+nn;   bc(k2)=0.0;     % x-derivative
k3=k1+2*nn; bc(k3)=0.0;     % y-derivative
bound=[bound;k1;k2;k3];
% right edge
k1=find(x==1);  bc(k1)=0.0; % value
k2=k1+nn;   bc(k2)=0.0;     % x-derivative
k3=k1+2*nn; bc(k3)=0.0;     % y-derivative
bound=[bound;k1;k2;k3];
%
bound=unique(bound);  % remove redundancies
bc=bc(bound);
return
