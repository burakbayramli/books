function [xp, newrad,idid,sdata,nf] = trfix(xc, f, hc, gc, fc, oldrad,ijob,jdata)
%
%
%     C. T. Kelley, Dec 15, 1997
%
%     This code comes with no guarantee or warranty of any kind.
%
%     function [xt, newrad, idid, sdata, nf] 
%                = trfix(xc, f, hc, gc, oldrad, ijob, jdata)
%
%     Figure out what the new trust region radius and new point are
%
%     This code is called by ntrust.m
%     There is no reason for you to call this directly.
%     
%     Input: xc = current point
%     f  = objective 
%     hc = current Hessian 
%     gc = current gradient
%     fc = current function value
%     oldrad = current TR radius
%     ijob = what to do now: 1 = fresh start
%                            2 = TR radius reduction in progress
%                            3 = attempt TR radius expansion
%     jdata = Newton direction when ijob = 1 or 2, avoids recomputation
%     nf = number of function evaluations 
%
%     Output: xp = new point
%     newrad = new TR radius
%     idid = result flag: 1 = step ok
%                         2 = TR radius reduced, step not ok
%                         3 = expansion attempted, save step and try
%                             to do better
%                         4 = expansion step failed, use the last good step
%     sdata = Newton direction to use in next call if idid > 1
%
%
%     Find the Cauchy point
%
%     bflag=1 means that the trial point is on the TR boundary and is not
%             the Newton point
%
nf=0;
bflag=0;
idid=1;
trrad=oldrad;
mu=gc'*hc*gc;
mu1=gc'*gc;
dsd=-gc; 
if ijob == 1
   dnewt=hc\dsd;
else
   dnewt=jdata;
end
sdata=dnewt;
if mu > 0
   sigma = mu1/mu;
   if(sigma*norm(gc)) > trrad
      sigma=trrad/norm(gc);
   end
   cp = xc-sigma*gc;
else
%
%     If steepest descent direction is a direction of negative curvature
%     take a flying leap to the boundary.
%
   bflag=1;
   sigma=trrad/norm(gc);
   cp=xc-sigma*gc;
end
%
%     If CP is on the TR boundary, that's the trial point.
%     If it's not, compute the Newton point and complete the dogleg.
%
if bflag==1
   xt=cp;
else
%
%     If we get to this point, CP is in the interior and the steepest
%     descent direction is a direction of positive curvature.
%
   dsd=-gc; dnewt=hc\dsd;
   xn=xc+dnewt;
   mu2=dsd'*dnewt;
%
%     If the Newton direction goes uphill, revert to CP.
%
   if mu2 <= 0
       xt=cp;
%
%     If the Newton point is inside, take it.
%
   elseif norm(dnewt) <= trrad
       xt=xn;
%
%    Newton is outside and CP is inside. Find the intersection of the
%    dog leg path with TR boundary.
%
   else
       d1=sigma*gc; d2=d1+dnewt;
       aco=d2'*d2; bco=-2*d1'*d2; cco= (d1'*d1) - trrad*trrad;
       xi=(-bco+sqrt((bco*bco) - 4*aco*cco))/(2*aco);
       xt=cp + xi*(xn-cp);
       bflag=1;
   end
end
%
%     Now adjust the TR radius using the trial point
%
st=xt-xc; ft=feval(f,xt); ared=ft-fc; nf=nf+1;
pred=gc'*st + .5* (st'*hc*st);
if ared/pred < .1
   xt=xc;
   trrad=norm(st)*.5;
   idid=2;
   if ijob == 3 idid = 4; end
elseif ared/pred > .75 & bflag==1
   trrad=trrad*2;
   disp('Expanding')
   idid=3;
end
newrad=trrad;
xp=xt;
