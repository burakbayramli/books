function [xp, fvalp, funp, fcost, iarm, levmar_hist, nfail, nunew] ...
         = lev_mar_exec(f, x, fun, jac, xc,  gc, ...
                iteration_data, nuold)
% LEV_MAR_EXEC
% v1.01
% ilev counter incremented as it should have been in the first place
% 1/10/2014
%
% Example of the executive_function option.
%
%function [xp, fvalp, funp, fcost, iarm, levmar_hist, nfail, nunew] ...
%         = lev_mar_exec(f, x, fun, jac, xc,  gc, ...
%                iteration_data, nuold)
%
% I/O follows the instructions in the manual/book.
%
% Get the options you need from iteration_data.
%
options=iteration_data.options;
h=iteration_data.h;
complete_history=options.complete_history;
core_data=iteration_data.core_data;
parallel=options.parallel;
least_squares=options.least_squares;
if least_squares == 0
    disp('lev_mar_exec solves nonlinear least squares problems.')
    disp('Please set the least_squares option to 1')
    disp('and make sure you have a least squares problem to solve.')
end
%
nfail=1;
nu = nuold;
%
% Now it's a plain vanilla Levenberg-Marquardt step.
%
iarm=0;
ilev=0;
ilevmax=5;
mf=length(fun);
funmat=[];
failvec=[];
xarray=[];
fcost=0;
ared=-1;
xp=x;
funp=fun;
n=length(x);
fvalp=fun'*fun/2;
%
% Gt the epsilon-inactive indices.
%
epsb=1.d-6;
ilist=max(( x > epsb) , (x < 1 - epsb));
pr=diag(ilist);
rjac=jac*pr;
grad=jac'*fun;
fval=fun'*fun/2;
sbinding=-(eye(n) - pr)*grad;
%
while ilev < ilevmax & ared < 0
   lmm=(nu*eye(n) + jac'*jac);
%
%  Compute Levenberg-Marquardt step as per Section 3.9.4.
%
   big_matrix=[jac*pr ; sqrt(nu)*pr];
   [mr,nr]=size(pr);
   big_rhs=[fun; zeros(mr,1)];
   lm_direction=-big_matrix\big_rhs;
   xt= x + lm_direction;
   xt = min(max(xt,0),1);
   step = xt-x;
%
%  Evaluate function at trial point.
%
   [funt, ifail, icount]=feval(f,xt,h,core_data);
%
% Get the data organized for the complete_history update.
%
   funmat=[funmat, funt];
   failvec=[failvec, ifail];
   xarray=[xarray, xt];
   ilev = ilev + 1;
%
%  If the function returns a value, then
%  adjust the Levenberg parameter using the trial step.
%
   if ifail == 0
%
      fvalt=.5*funt'*funt;
      fcost=fcost+icount;
      ared=fval-fvalt;
      pred = -step'*grad - step'*lmm*step/2;
      if ared < 0 
         nu = nu*2;
         iarm=iarm+1;
      else
         xp=xt;
         funp=funt; 
         fvalp=fvalt;
         nfail=0;
         if ared/pred > .75 
            nu=nu/2;
         end
         if ared/pred < .25
            nu=2*nu;
         end
         break;
      end
   else
      nu = nu*2;
      iarm=iarm+1;
   end
end
%
% Clean up after yourself.
%
levmar_hist=build_history(xarray,funmat,failvec);
%
nunew=nu;
