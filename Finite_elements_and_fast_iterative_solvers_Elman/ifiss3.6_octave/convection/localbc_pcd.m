function [bae,fe] = localbc_pcd(ae,fe,edges,xl,yl)
%LOCALBC_PCD imposes Dirichlet BC for Poisson error estimator 
%   [bae,fe] = localbc_pcd(ae,fe,edges,xl,yl);
%   input
%          ae        Poisson problem matrix
%          fe        rhs vector
%          edges     boundary edge vector 
%          xl        vertex x-coordinates  
%          yl        vertex y-coordinates  
%   output
%          bae       Poisson problem matrix
%          fe        rhs vector
%
%   calls function: specific_bc
%   IFISS function: DJS; 5 March 2005.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
global viscosity
bae=ae; ffe=fe;
nvtx = length(fe); nbd=length(edges);
zero_col=zeros(nvtx,1); zero_row=zeros(1,nvtx);
%
%% loop over boundary edges
for bd=1:nbd
    ek=edges(bd);
% compute boundary edge coordinates
    xbd(1)=xl(ek);xbd(3)=xl(ek+1);xbd(2)=0.5*(xbd(1)+xbd(3));
    ybd(1)=yl(ek);ybd(3)=yl(ek+1);ybd(2)=0.5*(ybd(1)+ybd(3));
% compute interpolated boundary error
    bc=specific_bc(xbd,ybd);
    error = bc(2)-0.5*(bc(1)+bc(3));
%% impose boundary condition without modifying the other equations
%% scale RHS with the viscosity
%% DJS/DK mod
%    fe = fe - error*viscosity*bae(:,ek);
    bae(:,ek)=zero_col; bae(ek,:)=zero_row;   
    bae(ek,ek)=1;  fe(ek)=error * viscosity; 
end
return
