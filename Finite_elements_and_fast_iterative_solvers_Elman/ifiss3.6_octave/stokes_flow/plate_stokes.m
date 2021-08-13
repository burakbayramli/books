%PLATE_STOKES set up flow problem in slit domain 
%   IFISS scriptfile: DJS; 27 May 2012. 
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
%% define geometry
pde=3; domain=4; enclosed=0;
%% define geometry
plate_domain
load plate_grid.mat
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
save plate_stokes_nobc.mat pde domain qmethod stretch A B Q f g xy xyp mbound bound x y 
save plate_stokes_nobc.mat Bx By  -append
if qmethod==1 
   save plate_stokes_nobc.mat C G ev ee ebound -append
elseif qmethod==0 
   save plate_stokes_nobc.mat C G ev ee ebound mv enclosed -append
elseif qmethod==2
   save plate_stokes_nobc.mat G map mv mp -append
else
   save plate_stokes_nobc.mat G mv ee -append
end
fprintf('system matrices saved in plate_stokes_nobc.mat ...\n')
