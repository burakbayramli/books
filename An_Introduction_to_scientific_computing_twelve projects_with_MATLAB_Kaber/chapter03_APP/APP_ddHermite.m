%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
function [xx,dd]=APP_ddHermite(Tab)
%Tab(i,1) contains the  points 
%Tab(i,2) contains the maximal derivative to be interpolated
%Tab(i,3:Tab(i,2))  contains the values of the function 
%and its   possible derivatives.
%
%build  dd, xx and ind
dd=[];ind=[];xx=[];
for i=1:size(Tab,1);
   xx=[xx;Tab(i,1)];dd=[dd;Tab(i,3)];ind=[ind;0];
   for j=1:Tab(i,2)
      xx=[xx;Tab(i,1)];ind=[ind;j];dd=[dd;Tab(i,j+3)/factorial(j)];
   end;
end;
%Compute the  divided differences
n=length(dd);
for p=1:n-1
   for k=n:-1:p+1
      if ind(k)
         ind(k)=ind(k)-1;
      else
         dd(k)=(dd(k)-dd(k-1-ind(k-1)))/(xx(k)-xx(k-p));
      end;
   end;
end;

     
