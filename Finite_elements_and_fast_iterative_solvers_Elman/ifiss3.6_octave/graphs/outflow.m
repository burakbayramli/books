function [total, uyref, yref] = outflow(qmethod,xy,xst,fig,mkr)
%OUTFLOW plots/explores tangential flow at outflow
%   [total, uyref, yref] = outflow(qmethod,xy,xst,fig,'.-r');
%   input
%          qmethod    mixed method 
%          xy         velocity nodal coordinate vector 
%          xst        flow solution vector 
%          fig        figure number
%          mkr        plotting symbol
%
%   IFISS function: DJS; 24 November 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy(:,1)); xref=max(xy(:,1));
ux=xst(1:nvtx); uy=xst(nvtx+1:2*nvtx); 
kk=find(xy(:,1)==xref);
uyref=uy(kk)'; uxref=ux(kk)'; yref=xy(kk,2);
figure(fig)
plot(yref,uyref,mkr), axis('square'), title('x-section of tangential flow')
hold on
%%
%% compute volume of flow using appropriate quadrature
nny=length(yref); hy=yref(2)-yref(1);
if qmethod >1,
%% Simpson's rule
ww=ones(nny,1); ww(2:2:nny-1)=4; ww(3:2:nny-1)=2;
total=(hy/3)*sum(ww.*uyref');
else 
%% Trapezium rule
ww=ones(nny,1); ww(2:2:nny-1)=2; ww(3:2:nny-1)=2;
total=(hy/2)*sum(ww.*uyref');
end
fprintf('\nvolume of tangential flow is %10.6e \n',total)
return
