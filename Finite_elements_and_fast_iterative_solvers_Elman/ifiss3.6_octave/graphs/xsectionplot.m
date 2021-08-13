function xsectionplot(domain,qmethod,xy,x_gal,yyref,fig,mkr)
%XSECTIONPLOT plots scalar solution on horizontal X-section 
%   xsectionplot(domain,qmethod,xy,x_gal,yyref,fig,'.b');
%   input
%          domain     domain identifier 
%          qmethod    approximation method 
%          xy         nodal coordinate vector 
%          x_gal      solution vector
%          yyref      y-location of grid line  
%          fig        figure number
%          mkr        plotting character
%
%   IFISS function: DJS; 2 March 2013.
% Copyright (c) 2012 D.J. Silvester, H.C. Elman, A. Ramage 
nvtx=length(xy(:,1)); 
kkall=find(abs((xy(:,2))-yyref)<10*eps); lkk=length(kkall);
kk=kkall; %kk=kkall(lkk-npts+1:lkk);
xref=xy(kk,1); yref=xy(kk,2);
fprintf('x-section analysis | y = %8.4e \n',yyref)
if isempty(kk), error('location yyref is not valid!'), end
uxref=x_gal(kk); 
figure(fig)
plot(xref,uxref,mkr), axis('square'), title('x-section solution')
return
