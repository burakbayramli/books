%SOLVE_OBSTACLE_NAVIER solve Navier-Stokes problem in obstacle domain
%   IFISS scriptfile: HCE; DJS;  20 September 2016.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage 
% Original code written by M. Wu, 2009

clear variables
fprintf('Incompressible flow problem on obstacle domain ...\n')
viscosity=default('viscosity parameter (default 1/150)',1/150);
nlmethod=default('Picard/Newton/hybrid linearization 1/2/3 (default hybrid)',3);
nlmethod=nlmethod-1;
if nlmethod==0,
   maxit_p=default('number of Picard iterations (default 9)',9);
   maxit_n=0;
elseif nlmethod==1,
   maxit_p=0;
   maxit_n=default('number of Newton iterations (default 6)',6);
else
   maxit_p=default('number of Picard iterations (default 3)',3);
   maxit_n=default('number of Newton iterations (default 5)',5);
end
tol_nl=default('nonlinear tolerance (default 1.1*eps)',1.1*eps);
%
%
%% initialize for nonlinear iteration: compute Stokes solution
%% load assembled matrices
gohome
cd datafiles
load obstacle_stokes_nobc.mat
%
fprintf('stokes system ...\n') 
%% boundary conditions
[Ast,Bst,fst,gst] = flowbc(A,B,f,g,xy,bound);
nlres0_norm = norm([fst;gst]);
%
nv=length(fst)/2; np=length(gst); 
if qmethod>1,
   beta=0;
   xst=[Ast,Bst';Bst,sparse(np,np)]\[fst;gst];
elseif qmethod==1
		beta=1/4;     % default parameter
   xst=[Ast,Bst';Bst,-beta*C]\[fst;gst];
elseif qmethod==0
   fprintf('computing pressure stabilized solution...\n')
   xst=[Ast,Bst';Bst,-C]\[fst;gst]; beta=1;
end
% compute residual of Stokes solution
if qmethod>1
   N = navier_q2(xy,mv,xst);
elseif qmethod<=1,
   nubeta=beta/viscosity;
   N = navier_q1(xy,ev,xst);
end
Anst = viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N];
[Anst,Bst,fst,gst] = flowbc(Anst,B,f,g,xy,bound);
if     qmethod>1,  nlres = [Anst,Bst';Bst,sparse(np,np)]*xst-[fst;gst];
elseif qmethod<=1, nlres = [Anst,Bst';Bst,-nubeta*C]*xst-[fst;gst];
end
nlres_norm  = norm(nlres);
%%% plot solution
spc=default('uniform/nonuniform streamlines 1/2 (default uniform)',1);
contourn = default('number of contour lines (default 50)',50);
flowplot09(qmethod,xst,By,Bx,A,xy,xyp,x,y,bound,bndxy,bnde,obs,contourn,spc,33)
pause(1) 
fprintf('\n\ninitial nonlinear residual is %e ',nlres0_norm)
fprintf('\nStokes solution residual is %e\n', nlres_norm)
flowsol = xst;
%
%
pde=4;
it_p = 0;
%
% nonlinear iteration 
%%% Picard startup step
while nlres_norm>nlres0_norm*tol_nl && it_p<maxit_p,
   it_p = it_p+1;
   fprintf('\nPicard iteration number %g \n',it_p),
% compute Picard correction and update solution
   if     qmethod>1,  dxns = -[Anst,Bst';Bst,sparse(np,np)]\nlres;
   elseif qmethod<=1, dxns = -[Anst,Bst';Bst,-nubeta*C]\nlres;
   end
   xns = flowsol + dxns;
% compute residual of new solution
   if     qmethod>1,  N = navier_q2(xy,mv,xns);
   elseif qmethod<=1, N = navier_q1(xy,ev,xns);
   end
   Anst = viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N];
   [Anst,Bst,fst,gst] = flowbc(Anst,B,f,g,xy,bound);
   if     qmethod>1,  nlres = [Anst,Bst';Bst,sparse(np,np)]*xns-[fst;gst];
   elseif qmethod<=1, nlres = [Anst,Bst';Bst,-nubeta*C]*xns-[fst;gst];
   end
   nlres_norm = norm(nlres);
   nnv=length(fst); soldiff=norm(xns(1:nnv)-flowsol(1:nnv));
   fprintf('nonlinear residual is %e',nlres_norm)
   fprintf('\n   velocity change is %e\n',soldiff)
% plot solution
   flowplot09(qmethod,xns,By,Bx,A,xy,xyp,x,y,bound,bndxy,bnde,obs,contourn,spc,66);drawnow
   pause(1)
   flowsol = xns;
% end of Picard iteration loop
end
%%%
%
it_nl = it_p;
it_n = 0;
%%%% Newton iteration loop
while (nlres_norm > nlres0_norm*tol_nl) && (it_nl < maxit_p + maxit_n),
   it_n = it_n+1;
   it_nl = it_nl+1;
   fprintf('\nNewton iteration number %g \n',it_n),
% compute Jacobian of current solution
   if     qmethod>1,  [Nxx,Nxy,Nyx,Nyy] = newton_q2(xy,mv,flowsol);
   elseif qmethod<=1, [Nxx,Nxy,Nyx,Nyy] = newton_q1(xy,ev,flowsol);
   end
   J = viscosity*A + [N + Nxx, Nxy; Nyx, N + Nyy];
   Jnst = newtonbc(J,xy,bound); 
% compute Newton correction and update solution
   if     qmethod>1,  dxns = -[Jnst,Bst';Bst,sparse(np,np)]\nlres;
   elseif qmethod<=1, dxns = -[Jnst,Bst';Bst,-nubeta*C]\nlres;
   end
   xns = flowsol + dxns;
% compute residual of new solution
   if     qmethod>1,  N = navier_q2(xy,mv,xns);
   elseif qmethod<=1, N = navier_q1(xy,ev,xns);
   end
   Anst = viscosity*A + [N, sparse(nv,nv); sparse(nv,nv), N];
   [Anst,Bst,fst,gst] = flowbc(Anst,B,f,g,xy,bound);
   if     qmethod>1,  nlres = [Anst,Bst';Bst,sparse(np,np)]*xns-[fst;gst];
   elseif qmethod<=1, nlres = [Anst,Bst';Bst,-nubeta*C]*xns-[fst;gst];
   end
   nlres_norm = norm(nlres);
   nnv=length(fst); soldiff=norm(xns(1:nnv)-flowsol(1:nnv));
   fprintf('nonlinear residual is %e',nlres_norm)
   fprintf('\n   velocity change is %e\n',soldiff)
% plot solution
   flowplot09(qmethod,xns,By,Bx,A,xy,xyp,x,y,bound,bndxy,bnde,obs,contourn,spc,66);drawnow
   pause(1)
   flowsol = xns;
%% end of Newton iteration loop 
end
%
if nlres_norm <= nlres0_norm * tol_nl, 
   fprintf('\nfinished, nonlinear convergence test satisfied\n');
else
   fprintf('\nfinished, stopped on iteration counts\n');
end
%
%% save the computed steady-state solution
fprintf('steady-state solution saved in obstacle_steadyflow.mat\n\n');
Usteady=flowsol(1:nnv);
save obstacle_steadyflow.mat Usteady soldiff nlres_norm viscosity
%
%%% estimate errors
if qmethod==1
   [jmpx,jmpy,els] = stressjmps_q1p0(viscosity,flowsol,xy,ev,ebound);
   [error_x,error_y,fex,fey,ae] = navierpost_q1p0_p(viscosity,flowsol,jmpx,jmpy,els,xy,ev);
   [error_x,error_y] = navierpost_q1p0_bc(viscosity,ae,fex,fey,...
                                        error_x,error_y,xy,ev,ebound);
   error_div = q1div(xy,ev,flowsol);
   errorest=sqrt(sum(error_x.^2 + error_y.^2 + error_div.^2));
   fprintf('estimated overall error is %10.6e \n',errorest)
   ee_error=sqrt((error_x.^2 + error_y.^2 + error_div.^2));
%% plot element errors
   %eplotl(ee_error,ev,xy,x,y,67);
%% plot macroelement errors
   mplotl(ee_error,ev,xy,x,y,67), title('Estimated error')
   pause(2), figure(66)
elseif qmethod==0
   error_div = q1div(xy,ev,flowsol);	
elseif qmethod>1, %navierpost,
end
