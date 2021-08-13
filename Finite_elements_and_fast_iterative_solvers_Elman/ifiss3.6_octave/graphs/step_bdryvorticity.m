function [wbound] = step_bdryvorticity(domain,qmethod,xy,bound,w,fig,mkr)
%STEP_BDRYVORTICITY plots vorticity solution on step boundary
%   [wbound] = step_bdryvorticity(domain,qmethod,xy,bound,w,19,'ob-');
%   input
%          domain     problem domain 
%          qmethod    mixed method 
%          xy         velocity nodal coordinate vector 
%          bound      boundary node index 
%          w          vorticity solution vector
%          fig        figure reference 
%          mkr        plotting character
%
%   IFISS function: DJS; 1 May 2012
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy(:,1)); nbound=length(bound);
fprintf('\n vorticity boundary analysis\n')
if domain~=3, error('function is only defined for step domain'), end
% identify boundary segments
k1=find(xy(bound,2)==0); 
k2=find(xy(bound,1)==0 & xy(bound,2)<0); k2=k2(end:-1:1);
k3=find(xy(bound,2)==-1 & xy(bound,1)>0);
% complete boundary
kk=[k1;k2;k3]; nkk=length(kk);wbound=w(bound(kk));
figure(fig)
plot(1:nkk,wbound,mkr), axis('square'), 
title('Entire lower boundary vorticity distribution','FontSize',12)
figure(fig+1)
kkb=[k2(end);k3]; nkkb=length(kkb); wb=w(bound(kkb));
plot(xy(bound(kkb),1),wb,mkr), axis('square'),
xlabel('x');
title('Lower boundary vorticity distribution','FontSize',12)
[wmin,km]=min(wb);
fprintf('minimum vorticity is %8.4e \n',wmin)
fprintf('           when x is  %8.4e \n',xy(bound(kkb(km)),1))
%
% specified boundary segment
endpoint=2;
kref=find(xy(bound,2)==-1 & xy(bound,1)<=endpoint);
xref=xy(bound(kref),1);
if xref(end)~=endpoint, error('Check: endpoint must be a grid point'), end
npts=length(xref); wref=w(bound(kref));
%
% compute mean vorticity using appropriate quadrature
hx=diff(xref);
if qmethod >1,
if 2*floor(npts/2)==npts, error('Check: endpoint must be a vertex'), end
% Simpson's rule
ww=ones(npts,1); ww(2:2:npts-1)=4*hx(1:2:npts-1); 
ww(1)=hx(1); ww(npts)=hx(npts-1);
ww(3:2:npts-2)=hx(2:2:npts-2)+hx(3:2:npts-2);
total=(1/3)*sum(ww.*wref);
else 
% Trapezium rule
ww=ones(npts,1); 
ww(1)=hx(1); ww(npts)=hx(npts-1);
ww(2:npts-1)=hx(1:npts-2)+hx(2:npts-1);
total=(1/2)*sum(ww.*wref);
end
fprintf('mean vorticity in [0,%g] is %8.4e \n',endpoint,total/endpoint)
return
