%SYMSTEP_STOKES set up flow problem in symmetric step domain
%   IFISS scriptfile: DJS; 19 November 2014.
% Copyright (c) 2014 D.J. Silvester, H.C. Elman, A. Ramage
clear variables
pde=3; enclosed=0;
%% define geometry
outbnd=default('horizontal dimensions [-1,L]: L? (default L=5)',5);
twostep_domain;
domain = 3; pde=3; enclosed=0; 
load step_grid.mat
%
%% set up matrices
q_in=default('Q1-Q1/Q1-P0/Q2-Q1/Q2-P1: 1/2/3/4? (default Q1-P0)',2);
qmethod=q_in-1;
if qmethod==2,
   [x,y,xy,xyp,mp,map] = q2q1gridx(x,y,xy,mv,bound);
   [A,B,Q,G,Bx,By,f,g] = stokes_q2q1(xy,xyp,mv,mp);
elseif qmethod==3,
   [x,y,xy,xyp,ee] = q2p1grid(x,y,xy,mv,bound);
   [A,B,Q,G,Bx,By,f,g] = stokes_q2p1(xy,xyp,mv);
elseif qmethod==0 
   [ev,ee,ebound,xyp] = q1q1grid(x,y,xy,mv,bound,mbound);
   [A,B,Q,C,G,Bx,By,f,g] = stokes_q1q1(xy,ev);
elseif qmethod==1 
   [ev,ee,ebound,xyp] = q1p0grid(x,y,xy,mv,bound,mbound);
   [A,B,Q,C,G,Bx,By,f,g] = stokes_q1p0(xy,xyp,mv,ev);
end
gohome
cd datafiles
save step_stokes_nobc.mat pde domain outbnd qmethod stretch A B Q f g xy xyp mbound bound x y 
save step_stokes_nobc.mat Bx By bndxy bnde obs -append
if qmethod==1 
   save step_stokes_nobc.mat C G ev ee ebound -append
elseif qmethod==0 
   save step_stokes_nobc.mat C G ev ee ebound mv enclosed -append
elseif qmethod==2
   save step_stokes_nobc.mat G mv mp map -append
else
   save step_stokes_nobc.mat G ee mv  -append
end
fprintf('system matrices saved in step_stokes_nobc.mat ...\n')
