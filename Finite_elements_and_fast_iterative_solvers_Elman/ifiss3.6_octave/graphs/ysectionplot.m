function ysectionplot(domain,qmethod,xy,x_gal,xxref,fig,mkr)
%YSECTIONPLOT plots scalar solution on vertical X-section
%   ysectionplot(domain,qmethod,xy,x_gal,xxref,fig,'.b');
%   input
%          domain     domain identifier 
%          qmethod    approximation method 
%          xy         nodal coordinate vector 
%          x_gal      solution vector
%          xxref      x-location of grid line
%          fig        figure number
%          mkr        plotting character
%
%   IFISS function: DJS; 26 September 2013.
% Copyright (c) 2013 D.J. Silvester, H.C. Elman, A. Ramage
nvtx=length(xy(:,1)); 
kkall=find(abs((xy(:,1))-xxref)<10*eps); lkk=length(kkall);
kk=kkall; %kk=kkall(lkk-npts+1:lkk);
xref=xy(kk,1); yref=xy(kk,2);
fprintf('x-section analysis | x = %8.4e \n',xxref)
if isempty(kk), error('location xxref is not valid!'), end
uyref=x_gal(kk);
figure(fig)
plot(yref,uyref,mkr), axis('square'), title('y-section solution')
return
