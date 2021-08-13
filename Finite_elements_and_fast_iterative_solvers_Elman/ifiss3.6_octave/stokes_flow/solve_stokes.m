%SOLVE_STOKES solve Stokes problem in square domain
%   IFISS scriptfile: DJS; 29 September 2013.
% Copyright (c) 2005 D.J. Silvester, H.C. Elman, A. Ramage 
clear variables
gohome
cd datafiles
load square_stokes_nobc.mat
%
fprintf('imposing (enclosed flow) boundary conditions ...\n') 
%% boundary conditions
[Ast,Bst,fst,gst] = flowbc(A,B,f,g,xy,bound);
%
np=length(gst); nu=length(f)/2; tic, 
%% compute solution
if qmethod>1
   beta=0;
%-------------------------------------------------------
% xst=[Ast,Bst';Bst,sparse(np,np)]\[fst;gst];
% stabilized version
xstz=[Ast,Bst',zeros(2*nu,1);Bst,sparse(np,np),ones(np,1)/np; ...
      zeros(1,2*nu),ones(1,np)/np,zeros(1,1)]\[fst;gst;0];
xst=xstz(1:end-1); multiplier=xstz(end);
%-------------------------------------------------------
elseif qmethod==1
   beta=default('stabilization parameter (default is 1/4)',1/4);
%-------------------------------------------------------
%  xst=[Ast,Bst';Bst,-beta*C]\[fst;gst];
% stabilized version
xstz=[Ast,Bst',zeros(2*nu,1);Bst,-beta*C,ones(np,1)/np; ...
      zeros(1,2*nu),ones(1,np)/np,zeros(1,1)]\[fst;gst;0];
xst=xstz(1:end-1); multiplier=xstz(end);
%-------------------------------------------------------
elseif qmethod==0
   fprintf('computing pressure stabilized solution...\n')
   beta=1;
%-------------------------------------------------------
%  xst=[Ast,Bst';Bst,-beta*C]\[fst;gst];
% stabilized version
xstz=[Ast,Bst',zeros(2*nu,1);Bst,-beta*C,ones(np,1)/np; ...
      zeros(1,2*nu),ones(1,np)/np,zeros(1,1)]\[fst;gst;0];
xst=xstz(1:end-1); multiplier=xstz(end);
%-------------------------------------------------------
end
etoc=toc; fprintf('Stokes system solved in %8.3e seconds\n',etoc) 
%
%%% plot solution
spc=default('uniform/nonuniform streamlines 1/2 (default uniform)',1);
flowplot(qmethod,xst,By,Bx,A,xy,xyp,x,y,bound,spc,33);
fprintf('\n') 
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
   eplot(ee_error,ev,xy,x,y,34,'Estimated energy error')
   pause(2), figure(33)
elseif qmethod==0 
   [jmpx,jmpy,els] = stressjmps_q1q1(1,xst,xy,ev,ebound);
   [error_x, error_y, fex, fey, ae] = stokespost_q1q1_p(xst,jmpx,jmpy,els,xy,ev);
   [error_x,error_y] = stokespost_q1q1_bc(ae,fex,fey,...                       
                                  error_x,error_y,xy,ev,ebound);
   error_div = q1div(xy,ev,xst);                          
   errorest=sqrt(sum(error_x.^2 + error_y.^2 + error_div.^2));
   fprintf('estimated overall error is %10.6e \n',errorest)
   ee_error=sqrt((error_x.^2 + error_y.^2 + error_div.^2));
   eplot(ee_error,ev,xy,x,y,34,'Estimated energy error')
   pause(2), figure(33)
elseif qmethod>1, stokespost, 
else 
   error_div = q2div(xy,mv,xst);	
end
