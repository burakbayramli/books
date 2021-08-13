%ITSOLVE_STOKES specialized iterative solution of Stokes problem
%optional preset parameters
%         apost        1/0 error estimation on/off  
%         stopit       1/0 stopping test on/off
% for further details see
%  "David J. Silvester and Valeria Simoncini",
%  "An Optimal Iterative Solver for Symmetric Indefinite
%         Systems stemming from Mixed Approximation",
% "{ACM} Transactions on Mathematical Software, 37, 2010."
% "http://eprints.ma.man.ac.uk/1450/"

%IFISS scriptfile: DJS; 21 January 2011. 
% Copyright (c) 2011 D.J. Silvester, V. Simoncini

% Declare global variables for scalar AMG preconditioner
global amg_grid amg_smoother number_vcycles
%
if pde~=3, error('You need to set up a discrete Stokes problem first!'), end
prob_type='stokes';
tol=1e-6; maxit=99;         %% fixed termination parameters
% set default options if not preset
if exist('apost','var')==0,
   apost=1;  stopit=1;       %% enforce dynamic stopping test
end   
%
% specify coefficient matrix
if qmethod<=1, stabC = beta*C;
       error('stabilized approximation is not implemented yet')
else,  stabC = sparse(np,np); end
%
% AMG velocity preconditioner
fprintf('Inexact AMG block preconditioning ..\n')
number_vcycles = default('number of V-Cycles? (default 1)',1);
amg_grid = amg_grids_setup(Ast);
fprintf('AMG with point damped Gauss-Seidel smoothing ..\n')
smoother_params = amg_smoother_params(amg_grid, 'PGS', 2);
amg_smoother = amg_smoother_setup(amg_grid, smoother_params);
MA='m_amgzz'; aparams = struct('A',Ast); 
% select pressure preconditioner and construct it
if qmethod==2;
   fprintf('Chebyshev iteration preconditioning ..\n')
   number_its = default('number of iterations? (default 10)',10);
   MQ='m_masscheb';qparams = struct('Q',Q,'its',number_its,'qmethod',-qmethod);
elseif qmethod==3;
   MQ='m_massdiag';qparams = struct('Q',Q,'qmethod',qmethod);
end 
%
if apost, %-------- error estimation disabled
   if qmethod==3;
   if domain == 3 || domain == 10 || domain==4,  % step, channel, obstacle
      fprintf('Natural outflow on right boundary ..\n')
      neumannb=out_Neumann_bound(mv,xy);
   else, fprintf('Enclosed flow ..\n'),  neumannb=[];
   end
  [hx,hy,eex] = eexgen(xy,xyp,mv,ee); % check element numbering
  [error_x,error_y,fex,fey,ae]= ...
                      stokespost_q2p1(xy,mv,mbound,neumannb,[fst;gst],eex,hx,hy);
   %% returns LDLT factorised element matrices ae
   Mest='est_errorq2p1'; 
   eparams = struct('ae',ae,'xy',xy,'mv',mv,'mbound',mbound, ...
                   'neumannb',neumannb,'eex',eex,'hx',hx,'hy',hy);
   else, error('Dynamic estimation not yet implemented!'), end
   [xest,resvec,infsup,errpde,info] = ...
       est_minres(Ast,Bst',stabC,fst,gst,apost,stopit,maxit,tol, ...
                  prob_type,MA,aparams,MQ,qparams,Mest,eparams);
   errorest=errpde(end);
   upperbd=[resvec*sqrt(2)/info(1)]'; kk=find(upperbd<errorest,1,'first');  
%  generate convergence plot
   figure
   inx=0:info(4)-1;rx=9:9:info(4);
%  fix legend
   semilogy(inx(1),sqrt(2)*resvec(1)./infsup(1),'x-r'); 
   hold on
   semilogy(inx,errpde,'-k');
   semilogy(inx(1),resvec(1),'o-b');
%  plot data
   semilogy(inx,sqrt(2)*resvec./infsup,'-r',rx-1,sqrt(2)*resvec(rx)./infsup(rx),'xr'); 
   semilogy(inx,resvec,'-b',rx-1,resvec(rx),'ob');
   semilogy([kk-1],[errpde(kk)],'*k','MarkerSize',9)
   hold off
   axis('square')
   xlabel('iteration number {\it k}')
   %title('h=1/16 (grid level 6)')
   h=legend('${\sqrt{2}/ \gamma_k^2}\> ||r_k||_{M_*}$', ...
          '$\eta_k$','$||r_k||_{M_*}$');
   set(h,'Interpreter','latex')
else  %-------- error estimation disabled
   [xest,resvec,infsup,errpde,info]= ...
        est_minres(Ast,Bst',stabC,fst,gst,apost,stopit,maxit,tol, ...
                   prob_type,MA,aparams,MQ,qparams);
%  plot data
   figure
   inx=0:info(4)-1;rx=9:9:info(4);
   semilogy(inx,sqrt(2)*resvec./infsup,'-r');
   hold on
   semilogy(inx,resvec,'-b');
   hold off
   axis('square')
   xlabel('iteration number {\it k}')
   title('MINRES convergence history')
   h=legend('${\sqrt{2}/ \gamma_k^2}\> ||r_k||_{M_*}$','$||r_k||_{M_*}$');
   set(h,'Interpreter','latex')
end
