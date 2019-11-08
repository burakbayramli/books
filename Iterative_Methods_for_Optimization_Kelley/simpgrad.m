function [sgr,fb,xb,sflag]=simpgrad(x,f,v,fc,fdiff)
%
% simplex gradient for use with implicit filtering
% also tests for best point in stencil
%
% set fdiff = 1 to get forward differencing, useful in Nelder-Mead
%               simplex condition/gradient computaiton
%
% omit fdiff or set to 0 in typical implicit filtering mode
%
%   compute the simplex gradient
%
%   Output: sgr = simplex gradient
%           fb  = best value in stencil
%           xb  = best point in stencil
%           sflag = 0 if (1) you're using central diffs and 
%                        (2) center of stencil is best point
%           sflag is used to detect stencil failure
%         
%
n=length(x); delp=zeros(n,1); delm=zeros(n,1);
xb=x; fb=fc; sflag=0;
for j=1:n;
   xp=x+v(:,j); xm=x-v(:,j); fp=feval(f,xp); delp(j)=fp-fc;
   if fp < fb fb=fp; xb=xp; sflag=1; end
   if fdiff==0 fm=feval(f,xm); delm(j)=fc-fm; 
      if fm < fb fb=fm; xb=xm; sflag=1; end
   end;
end
if fdiff==1 
   sgr=v'\delp; 
else
   sgr=.5*((v'\delp)+(v'\delm));
end
if fdiff==1
    xb=x; fb=fc; sflag=1;
end
