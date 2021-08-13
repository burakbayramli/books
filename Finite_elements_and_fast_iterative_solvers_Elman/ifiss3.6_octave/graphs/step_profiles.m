%STEP_PROFILES generates flow profiles for symmetric step domain
%   IFISS scriptfile: DJS; 20 November 2014.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage
if exist('x','var')==0,
   error('You need to set up a step-shaped domain first!'), 
end
varsteplen = max(x);
[uxref, uyref] = flowvolume(qmethod,xy,xns,-1,99,'.-c',0);, pause(1)
hold on
[uxref, uyref] = flowvolume(qmethod,xy,xns,0,99,'.-b',0); pause(1)
[uxref, uyref] = flowvolume(qmethod,xy,xns,1,99,'.-k',0); pause(1)
[uxref, uyref] = flowvolume(qmethod,xy,xns,3,99,'.-r',0); pause(1)
[uxref, uyref] = flowvolume(qmethod,xy,xns,5,99,'.-m',0);
legend('x=-1','x=0','x=1','x=3','x=5')
hold off
