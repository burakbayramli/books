%SOLVE_STEP_STOKES solve Stokes problem in step domain
%   IFISS scriptfile: DJS; 28 September 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
gohome
cd datafiles
load step_stokes_nobc.mat
%
%
fprintf('imposing boundary conditions and solving system ...\n') 
%% boundary conditions
[Ast,Bst,fst,gst] = flowbc(A,B,f,g,xy,bound);
%
np=length(gst); tic
%% compute solution
if qmethod==2
   beta=0;
   xst=[Ast,Bst';Bst,sparse(np,np)]\[fst;gst];
elseif qmethod==3
   beta=0;
   xst=[Ast,Bst';Bst,sparse(np,np)]\[fst;gst];
elseif qmethod==1
   beta=default('stabilization parameter (default is 1/4)',1/4);
   xst=[Ast,Bst';Bst,-beta*C]\[fst;gst];
elseif qmethod==0
   fprintf('computing pressure stabilized solution...\n')
   xst=[Ast,Bst';Bst,-C]\[fst;gst]; beta=1;
end
etoc=toc; fprintf('Stokes system solved in %8.3e seconds\n\n',etoc) 
%%% plot solution
flowplotl(qmethod,xst,By,Bx,A,xy,xyp,x,y,bound,33);
%
%%% estimate errors
if qmethod==1
   [jmpx,jmpy,els] = stressjmps_q1p0(1,xst,xy,ev,ebound);
   [error_x, error_y, fex, fey, ae] = stokespost_q1p0_p(jmpx,jmpy,els,xy,ev);
   [error_x,error_y] = stokespost_q1p0_bc(ae,fex,fey,...
                                  error_x,error_y,xy,ev,ebound);
   error_div = q1div(xy,ev,xst);
   errorest=sqrt(sum(error_x.^2 + error_y.^2 + error_div.^2));
   fprintf('estimated overall error is %10.6e \n',errorest)
   ee_error=sqrt((error_x.^2 + error_y.^2 + error_div.^2));
   eplotl(ee_error,ev,xy,x,y,34,'Estimated error')
   pause(5), figure(33)
elseif qmethod == 0
   [jmpx,jmpy,els] = stressjmps_q1q1(1,xst,xy,ev,ebound);
   [error_x, error_y, fex, fey, ae] = stokespost_q1q1_p(xst,jmpx,jmpy,els,xy,ev);
   [error_x,error_y] = stokespost_q1q1_bc(ae,fex,fey,...                       
                                  error_x,error_y,xy,ev,ebound);
   error_div = q1div(xy,ev,xst);
   errorest=sqrt(sum(error_x.^2 + error_y.^2 + error_div.^2));
   fprintf('estimated overall error is %10.6e \n',errorest)
   ee_error=sqrt((error_x.^2 + error_y.^2 + error_div.^2));
   eplotl(ee_error,ev,xy,x,y,34,'Estimated error')
   pause(5), figure(33)
elseif qmethod>1,stokespost,
else
   error_div = q2div(xy,mv,xst);	
end
