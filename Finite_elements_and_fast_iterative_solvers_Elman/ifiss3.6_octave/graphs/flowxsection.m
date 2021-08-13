function [uxref, uyref] = flowxsection(domain,qmethod,xy,xns,yyref,fig,mkr)
%FLOWXSECTION plots flow solution on horizontal X-section
%   [uxref, uyref] = flowxsection(domain,qmethod,xy,xns,yyref,fig,'.b');
%   input
%          domain     domain identifier 
%          qmethod    mixed method 
%          xy         velocity nodal coordinate vector 
%          xns        flow solution vector
%          yyref      y-location of grid line  
%          fig        figure number
%          mkr        plotting character
%   output
%          uxref      x-section horizontal flow vector 
%          uyref      x-section vertical flow vector 
%
%   IFISS function: DJS; 26 October 2013.
% Copyright (c) 2013 D.J. Silvester, H.C. Elman, A. Ramage
nvtx=length(xy(:,1)); npts=sqrt(nvtx);
if domain~=1, error('inappropriate domain specified!'), end
kkall=find(xy(:,2)<=yyref+10*eps); lkk=length(kkall);
kk=kkall(lkk-npts+1:lkk);
xref=xy(kk,1); yref=xy(kk,2);
fprintf('\nx-section analysis | y = %8.4e \n',yref(npts))
if isempty(kk), error('location yyref is not valid!'), end
uxref=xns(kk);  uyref=xns(nvtx+kk); 
figure(fig)
subplot(121)
plot(xref,uxref,mkr), axis('square'), title('x-section horizontal velocity')
subplot(122)
plot(xref,uyref,mkr), axis('square'), title('x-section vertical velocity')
%%
%% compute volume of flow using appropriate quadrature
hx=diff(xref);
if qmethod >1,
%% Simpson's rule
ww=ones(npts,1); ww(2:2:npts-1)=4*hx(1:2:npts-1); 
ww(1)=hx(1); ww(npts)=hx(npts-1);
ww(3:2:npts-2)=hx(2:2:npts-2)+hx(3:2:npts-2);
total=(1/3)*sum(ww.*uyref);
else 
%% Trapezium rule
ww=ones(npts,1); 
ww(1)=hx(1); ww(npts)=hx(npts-1);
ww(2:npts-1)=hx(1:npts-2)+hx(2:npts-1);
total=(1/2)*sum(ww.*uyref);
end
fprintf('x-section flow volume is %8.4e \n',total)
return
