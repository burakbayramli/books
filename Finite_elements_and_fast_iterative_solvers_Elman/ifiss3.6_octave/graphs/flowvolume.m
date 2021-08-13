function [uxref, uyref] = flowvolume(qmethod,xy,xns,xref,fig,mkr,bothplot)
%FLOWVOLUME plots flow solution on vertical X-section 
%   [uxref, uyref] = flowvolume(qmethod,xy,xns,xref,fig,'.-b',0);
%   input
%          qmethod    mixed method 
%          xy         velocity nodal coordinate vector 
%          xns/xst    flow solution vector
%          xref       x-location of grid line  
%          fig        figure number
%          mkr        plotting character
%          bothplot   one/two component subplot switch
%   output
%          uxref      x-section horizontal flow vector 
%          uyref      x-section vertical flow vector 
%
%   IFISS function: DJS; 1 May 2012.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy(:,1)); 
kk=find(xy(:,1)==xref);
if isempty(kk), error('location xref is not a grid-line!'), end
fprintf('\nX-section analysis | x = %8.4e \n',xref)
yref=xy(kk,2);
uxref=xns(kk);  uyref=xns(nvtx+kk); 
npts=length(uxref);
figure(fig)
if bothplot==1,
subplot(121)
plot(yref,uxref,mkr), axis('square'),
title('X-section horizontal velocity','FontSize',12)
subplot(122)
plot(yref,uyref,mkr), axis('square'), 
title('X-section vertical velocity','FontSize',12)
else
plot(yref,uxref,mkr), axis('square'), 
title('X-section velocity profile','FontSize',12)
end
%%
%% compute volume of flow using appropriate quadrature
hy=diff(yref);
if qmethod >1,
%% Simpson's rule
ww=ones(npts,1); ww(2:2:npts-1)=4*hy(1:2:npts-1); 
ww(1)=hy(1); ww(npts)=hy(npts-1);
ww(3:2:npts-2)=hy(2:2:npts-2)+hy(3:2:npts-2);
total=(1/3)*sum(ww.*uxref);
else 
%% Trapezium rule
ww=ones(npts,1); 
ww(1)=hy(1); ww(npts)=hy(npts-1);
ww(2:npts-1)=hy(1:npts-2)+hy(2:npts-1);
total=(1/2)*sum(ww.*uxref);
end
fprintf('X-section flow volume is %8.4e \n',total)
return
