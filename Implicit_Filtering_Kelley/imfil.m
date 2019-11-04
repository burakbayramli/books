function  [x,histout,complete_history]=imfil(x0,f,budget,bounds,options,...
                                             extra_arg)
% IMFIL v1.02
% Minimization of noisy functions subject to explicit bound
% constraints + hidden constraints
%
% function  [x,histout,complete_history]=imfil(x0,f,budget,bounds,options,...
%                                              extra_arg)
%
%
% Input: 
%       x0 = initial iterate.
%        f = objective function.
%            The calling sequence for f should be
%            [fout,ifail,icount]=f(x,h)
%
%             h is an optional argument, and should be used only if your
%             function is scale-aware, ie does something useful with 
%             the scale, such as tuning accuracy.
%
%             fout = f(x)
%
%             fail = 0 unless the evaluation of f fails, in which case
%                    set ifail=1 and fout = NaN
%
%             icount = number of calls to the expensive part of f
%                     f should be smart enough to not do anything
%                     if a bound constraint or a cheap hidden constraint
%                     is violated
%
%             f may return a fourth argument noise_level if f is 
%             noise aware. RTFM about this one.
%
%   budget = max cost, uses icount from f(x)
%            The iteration will terminate after the iteration that
%            exhausts the budget. This argument is required.
%            Suggestion: try 10*N^2. 
%
% bounds   = [low, high] = N x 2 array of bound constraints on the variables. 
%            This is REQUIRED. If you want to solve an unconstrained
%            problem, you must do so without the scaling we do in
%            f_internal, and using imfil_core is currently the only way 
%            to do that. The lower bounds are the first column;
%            the upper bounds are the second.
%
%            I plan let you solve unconstrained problems by setting
%            the bounds to Inf or -Inf. That will take some time while
%            I figure out how I want to do the scaling (or make you
%            do it).
%
%  options = options structure
%            Documentation on the way. In the meantime, RTFM or 
%            look at imfil_optset.m.
%
% extra_arg = optional extra argument to be passed to f
%
% Output: 
%        x = estimated minimizer
%
%  histout = iteration history, updated after each nonlinear iteration
%          = (number of iterations )x (N+5) array, the rows are
%             [fcount, fval, norm(sgrad), norm(step), iarm, xval]
%             fcount = cumulative function evals
%             fval = current function value
%             norm(sgrad) = current projected stencil grad norm
%             norm(step) = norm of last step
%             iarm=line searches within current iteration
%                 =-1 means first iterate at a new scale
%             xval = transpose of the current iteration
%
% complete_history = complete evaluation history
%
% complete_history is a structure with the data on every evaluation of f.
%
% complete_history.good_points has the successful points for columns.
% complete_history.good_values is a $M x N$ matrix  of the values at
%              the good points. M > 1 for least squares.
%              good_values=f(good_points)
%
% complete_history.failed_points has the unsuccessful points for columns.
%
% You may want to use this to build surrogates, decide to add new points to
% the stencil, or for troubleshooting. The complete history can take up a
% lot of room. I will only return it as output if you ask for it, and will
% not store it at all if you set the complete_history option to 'off'.
% If I don't store it at all, then your functions to add directions to the
% stencil can't use that data.
%
% C. T. Kelley, July 19, 2010.
% This code comes with no guarantee or warranty of any kind.
%

global imfil_fscale 
fname=f;
qbounds=bounds;
dbounds=bounds(:,2)-bounds(:,1);
%
% No options? Use the defaults.
%
if nargin < 5
    options=imfil_optset;
end
%
% If you asked for an extra argument, I will bury it in the options 
% structure
%
if nargin == 6
  options=imfil_optset('extra_argument',1,'extra_arg_value',extra_arg,options);
end
n=length(x0);
%
% If the smooth_problem option is on, fix the dependencies and
% change the scales.
%
imfil_smooth_problem = imfil_optread('smooth_problem',options);
imfil_complete_history = imfil_optread('complete_history',options);
if imfil_smooth_problem == 1
disp('smooth problem on');
   bscales=[.5, .01, .001, .0001, .00001];
   options=imfil_optset(...
              'custom_scales',bscales,...
              'stencil_wins','yes',...
              'limit_quasi_newton','off',...
              'armijo_reduction',.25,...
              'maxitarm',5,options);
end
%
% Load the fun_data structure so f_internal will know about the scaling.
%
imfil_fscale      = imfil_optread('fscale',options);
fun_data=struct('fname',f,'qbounds',qbounds,'dbounds',dbounds,...
                'fun_fscale',imfil_fscale);
core_data=struct('options',options,'fun_data',fun_data);
%
% Scale the initial iterate to [0,1] before shipping to imfil_core.
% imfil_core uses 0 and 1 as the bounds for optimization. 
%
badargs = imfil_error_check('bounds',x0,bounds);
if badargs == 1
   disp(' Please fix this error and try running imfil again. ');
   x=x0; histout=[]; complete_history=[];
else
z0=(x0 - qbounds(:,1))./dbounds;
[z,histout,complete_history,ifail] = ...
                 imfil_core(z0,@f_internal,budget,core_data,bounds);
%
% Unscale the results to original bounds.
%
if ifail == 0
   x=(dbounds.*z)+qbounds(:,1);
   db=diag(dbounds);
   qb=diag(qbounds(:,1));
   xout=histout(:,6:n+5); 
   xlow=ones(size(xout))*qb;
   histout(:,6:n+5)=xout*db+xlow;
   xout=histout(:,6:n+5); 
end
%
% Unscale the complete history
%
if nargout == 3 & imfil_complete_history == 1 & ifail == 0
[mg,ng]= size(complete_history.good_points);
%
% Correct scaling for nonlinear least squares problems.
%
least_squares=imfil_optread('least_squares',options);
if least_squares==0
   hist_scale=imfil_fscale;
else
   hist_scale=sqrt(imfil_fscale);
end
complete_history.good_values=hist_scale*complete_history.good_values;
if mg > 0
   clow=qb*ones(size(complete_history.good_points));
   complete_history.good_points=db*complete_history.good_points+clow;
end
[mf,nf]= size(complete_history.failed_points);
if mf > 0
   flow=qb*ones(size(complete_history.failed_points));
   complete_history.failed_points=db*complete_history.failed_points+flow;
end
end
%
% Unscale the function and gradients.
%
if ifail == 0
histout(:,2)=histout(:,2)*imfil_fscale;
histout(:,3)=histout(:,3)*imfil_fscale;
end
end
if ifail == 1
disp(' Please fix this error and try running imfil again. ');
x=x0; histout=[]; complete_history=[];
end

function [fx,iff,icf,tol]=f_internal(x,h,core_data)
% F_INTERNAL
% Creates an O(1) dummy function with unit bounds.
% So, (I hope) the gradient and stencil are reasonably scaled.
%
% I encode scale and noise awareness in this function, too. If your
% function is not noise aware, I pass a zero noise back to imfil_core.
% imfil_core will do the right thing after that.
%
global imfil_fscale 
%
options = core_data.options;
imfil_scale_aware       = options.scale_aware;
imfil_noise_aware       = options.noise_aware;
imfil_least_squares     = options.least_squares;
imfil_simple_function   = options.simple_function;
imfil_extra_argument    = options.extra_argument;
exarg                   = options.extra_arg_value;
fun_data=core_data.fun_data;
fname=fun_data.fname;
qbounds=fun_data.qbounds;
dbounds=fun_data.dbounds;
%
[mx,nx]=size(x);
z=x;
for ix=1:nx
   z(:,ix)=(dbounds.*x(:,ix))+qbounds(:,1);
end
func_type=imfil_noise_aware + 10*imfil_scale_aware + ...
          100*imfil_simple_function + 1000*imfil_extra_argument;
switch func_type
   case 0
     [fx,iff,icf]=feval(fname,z);
     tol=0;
   case 10
     [fx,iff,icf]=feval(fname,z,h);
     tol=0;
   case 1
     [fx,iff,icf,tol]=feval(fname,z);
   case 11
     [fx,iff,icf,tol]=feval(fname,z,h);
   case 100
     fx=feval(fname,z);
     [mz,nz]=size(z);
     iff=zeros(nz,1); icf=nz*ones(nz,1); tol=0;
   case 110
     fx=feval(fname,z,h);
     [mz,nz]=size(z);
     iff=zeros(nz,1); icf=nz*ones(nz,1); tol=0;
   case 101
     [fx,tol]=feval(fname,z);
     [mz,nz]=size(z);
     iff=zeros(nz,1); icf=nz*ones(nz,1); tol=0;
   case 111
     [fx,tol]=feval(fname,z,h);
     [mz,nz]=size(z);
     iff=zeros(nz,1); icf=nz*ones(nz,1); tol=0;
   case 1000
     [fx,iff,icf]=feval(fname,z,exarg);
     tol=0;
   case 1010
     [fx,iff,icf]=feval(fname,z,h,exarg);
     tol=0;
   case 1001
     [fx,iff,icf,tol]=feval(fname,z,exarg);
   case 1011
     [fx,iff,icf,tol]=feval(fname,z,h,exarg);
   case 1100
     fx=feval(fname,z,exarg);
     [mz,nz]=size(z);
     iff=zeros(nz,1); icf=nz*ones(nz,1); tol=0;

   otherwise
     disp('internal error in imfil.m: f_internal');
end
[mf,nf]=size(fx);
%
% If this is the first time you evalute f and if imfil_fscale < 0,
% we change imfil_fscale to the correct relative scaling factor. This
% also makes imfil_fscale > 0 so we only compute the scaling once.
%
if imfil_fscale <= 0
   if imfil_fscale == 0
      imfil_fscale = -1.2;
   end
   if imfil_least_squares == 1
      val = fx(:,1)'*fx(:,1)/2;
      scale_base=val;
   else
      scale_base=max(abs(fx(1)));
   end
   if scale_base == 0
      imfil_fscale = 1;
   else
      imfil_fscale=abs(imfil_fscale)*scale_base;
   end
end
%
% Scale the function value. 
%
if imfil_least_squares == 0
    fx=fx/imfil_fscale;
else
    fx=fx/sqrt(imfil_fscale);
end
tol=tol/imfil_fscale;
function [x,histout,complete_history,ifailed] = ...
                 imfil_core(x0,f,budget,core_data,bounds)
% IFFCO_CORE v1.01
% Core code for imfil.m
% Minimization of f(x) subject to explict bound constraints
%
%
% function [x, histout,complete_history] ...
%                    = imfil_core(x0,f,budget,core_data,bounds)
%
% Bound constrained, parallel, implicit filtering code.
% 
% IMPLICIT FILTERING with SR1 and BFGS quasi-Newton methods
%
% Input: 
%       x0 = initial iterate
%        f = objective function,
%
%            the calling sequence for f should be 
%            [fout,ifail,icount]=f(x,h)
%
%            h is an optional argument for f, and should be used only if your
%            function is scale-aware, ie does something useful with 
%            the scale, such as tuning accuracy.
%
%  budget = upper limit on function evaluations. The optimization
%                 will terminate soon after the function evaluation counter
%                 exceeds the budget.
%
%  core_data = structure with the options + fun_data
%              The options are documented in optset.m. 
%              fun_data is private to imfil.m and is used in the internal
%              scaling for the function evaluation.
%
%  bounds  = N x 2 array of bound constraints on the variables.
%            These are the original bounds for the problem. We only
%            use these if you use the add_new_directions option. 
%
% Output: 
%        x = estimated minimizer
%  histout = iteration history, updated after each nonlinear iteration 
%          = N+five column array, the rows are
%            [fcount, fval, norm(sgrad), norm(step), iarm, xval]
%            fcount = cumulative function evals
%            fval = current function value
%            norm(sgrad) = current (projected) simplex grad norm
%                        = -1 means no gradient for this xval, this 
%                 can happen, for example, if you hit the target before you
%                 evaluate the simplex derivative
%            norm(step) = norm of last step 
%                       = 0 means no change, eg for stencil failure
%            iarm=line searches in current iteration to move to new point
%                =-1 means first iterate at a new scale or that the 
%                    inner iteration was terminated before the line search,
%                    eg for stencil failure
%             xval = transpose of the current iteration
%
% complete_history = complete evaluation history
%
% complete_history is a structure with the data on every evaluation of f.
% complete_history.good_points has the successful points for columns.
% complete_history.good_values is a $M x N$ matrix  of the values at
%              the good points. M > 1 for least squares.
%              good_values=f(good_points)
%
% complete_history.failed_points has the unsuccessful points for columns.
%
% You may want to use this to build surrogates, decide to add new points to
% the stencil, or for troubleshooting. The complete history can take up a
% lot of room. I will only return it as output if you ask for it, and will
% not store it at all if you set the complete_history option to 'off'.
% If I don't store it at all, then your functions to add directions to the
% stencil can't use that data.
%
% ifailed = total failure flag. I print a error message if this happens.
%
% C. T. Kelley, July 16, 2010
% This code comes with no guarantee or warranty of any kind.
%
global imfil_fscale
%
fcount=0; 
ifailed=0;
options=core_data.options;
%
% set up the difference scales and options
%
n=length(x0);
%
% In imfil_core the bounds are 0 and 1 on all the variables.
% We use the real bounds only if you use the add_new_directions
% option.
%
obounds=zeros(n,2); obounds(:,2)=1;
%
% Get the options we need.
%
imfil_explore= imfil_optread('explore',options);
imfil_executive = imfil_optread('executive',options);
imfil_function_delta = imfil_optread('function_delta',options);
imfil_least_squares  = imfil_optread('least_squares',options);
imfil_maxit          = imfil_optread('maxit',options);
imfil_maxitarm       = imfil_optread('maxitarm',options);
imfil_maxfail        = imfil_optread('maxfail',options);
imfil_noise_aware    = imfil_optread('noise_aware',options);
imfil_scale_aware    = imfil_optread('scale_aware',options);
imfil_target         = imfil_optread('target',options);
imfil_termtol        = imfil_optread('termtol',options);
stencil_wins   = imfil_optread('stencil_wins',options);
verbose        = imfil_optread('verbose',options);
imfil_complete_history = imfil_optread('complete_history',options);
complete_history=struct('good_points',[],'good_values',[],...
                            'failed_points',[]);
%
% The explore option may require some setup.
%
[explore_function, explore_data_flag, explore_data] = setup_explore(options);
%
%
% Initialize the iteration; create the stencil; set up the scales; ...
%
x=x0; xold=x0; n=length(x0); histout = [];
dscal=imfil_create_scales(options); nscal=length(dscal);
imfil_exec = options.executive;
if imfil_exec == 1
    hess = options.executive_data;
else
    hess=eye(n); 
end
xc=x0; ns=0; failc=0;
stop_now=0;
fval=imfil_target+1;
%
% Sweep through the scales.
%
sflag=1;
while (ns < nscal && fcount <= budget && failc < imfil_maxfail ...
       && stop_now == 0  && fval > imfil_target)
    ns=ns+1; h=dscal(ns); 
%
% Evaluate the function to test for instant termination and (if
% noise_aware is on) to get the estimate of the noise. Both the noise
% and the value of f  may vary as a function of the scale, so we 
% have to test it every time. 
%
% We could move this outside of the main loop if noise_aware and scale_aware
% are both off. In any case, we don't have to reevaluate f after a stencil
% failure unless f is scale-aware.
%
if imfil_noise_aware > 0 || fcount == 0 || imfil_scale_aware > 0
    [funs,iff,icf,noise_val]=feval(f,x,h,core_data);
    fval=f_to_vals(funs,imfil_least_squares);
    icount=icf;
else
    icount=0;
end
%
% The first call to f is the place to check for sanity.
%
% The first call to f also sorts out the scaling. So you can't scale the
% the targets, errors, or deltas before that call. 
%
    if fcount==0
       funerr=imfil_error_check('first_eval',funs,options,iff);
       if funerr == 1
          ifailed = 1;
          x=x0;
          histout = [];
          complete_history=[];
          break;
       end
       imfil_target=imfil_target/imfil_fscale;
       imfil_function_delta = imfil_function_delta/imfil_fscale;
       histout = [histout', [1, fval, 0, 0, 0, x']']';
%
% Initialize the internal data structures.
%
       itc=0;
       stencil_data = ...
           create_stencil_data(options, imfil_fscale, noise_val, bounds);
       iteration_data=struct('h',h,'obounds',obounds,'itc',itc, ...
               'xb',x,'fobjb',fval,'funsb',funs,...
               'complete_history',complete_history,...
               'f_internal',f,'core_data',core_data,'options',options);
    else
%
% End of first-call-to-f block
%
% If it's not the first call to f, update the internal data structures.
%
       iteration_data.h=h;
       stencil_data.noise_val = noise_val;
    end
    if imfil_complete_history > 0
       complete_history=iteration_data.complete_history;
       complete_history = ...
            single_point_hist_update(complete_history,x,funs,iff);
       iteration_data.complete_history=complete_history;
    end
    fcount=fcount+icf;
    if fval < imfil_target 
       stop_now=1;
       break;
    end
    stol=imfil_termtol*h; iarm=0; nfail=0;
%
% Compute the stencil gradient to prepare for the quasi-Newton iteration.
%
    [sdiff,sgrad,npgrad,fcount,sflag,jac,iteration_data,stop_now]...
          = manage_stencil_diff(x,f,funs,...
                  iteration_data,fcount,stencil_data,stop_now);
    histout = [histout', [fcount, fval, npgrad, -1, -1, x']']';
    gc=sgrad;
%
%
    if npgrad < stol || sflag==0 || stop_now==1 
%
%   Declare convergence at this scale on stencil failure or tolerance match.
%
       gc=sgrad; 
       if sflag ~= 0 
           failc=failc+1; 
       else
           failc=0;
       end
%
    else
%
%   Take a few quasi-Newton iterates. This is the inner iteration.
%
    failc=0;
%
% itc = inner iteration counter
%
    itc=0; 
%
% Newton while loop
%
    while itc < imfil_maxit*n && fval > imfil_target &&...
              npgrad >= stol && nfail==0 && fcount < budget ...
              && sflag > 0
    itc=itc+1;
    iteration_data.itc = itc;
    fc=fval;
%
%     Take an inner iteration. sdiff is the simplex derivative
%     (gradient or Jacobian) at x. gc is the simplex gradient at xc.
%
    [xp, fval, funs, fcount, iarm, iteration_data, nfail, hess] = ...
        imfil_inner_iteration(f, x, funs, sdiff, xc, gc, ...
             iteration_data, hess, fcount);
%
% Stop the entire iteration if you've hit the target.
%
    if fval < imfil_target 
       stop_now=1;
       x=xp;
       stepn=norm(xold-x,inf); 
       histout = [histout', [fcount, fval, npgrad, stepn, iarm, x']']';
       break;
    end
%
% Update xold, xc, and x. At this point the model Hessian and gc
% are evaluated at xc. xc and gc only get updated right here.
%
     xold=x; xc=x; gc=sgrad; x=xp;
%
%
% If stencil_wins is on, then take the best point you have. 
% You'll update x, but not xc.
%
     if  stencil_wins == 1
         [x, funs, fval] = write_best_to_x (iteration_data);
         stepn=norm(xc-x,inf); 
         histout = [histout', [fcount, fval, npgrad, stepn, iarm, x']']';
         nfail=0;
     end
%
% Stop on small objective function changes?
%
     fdelta=abs(fval-fc);
     if fdelta > 0 && fdelta < imfil_function_delta 
        stop_now=1;
        sflag=0;
        stepn=norm(xc-x,inf); 
        histout = [histout', [fcount, fval, npgrad, stepn, iarm, x']']';
        break;
     end
%
%    Compute the difference gradient for the next nonlinear iteration.
%
     if stop_now == 0 && fcount < budget
%       gc=sgrad;
       [sdiff,sgrad,npgrad,fcount,sflag,jac,iteration_data,stop_now]...
               = manage_stencil_diff(x,f,funs,...
                         iteration_data,fcount,stencil_data,stop_now);
     end
%
% If the quasi-Newton method terminated successfully or with a
% stencil failure, make sure x is now the best point.
%
     if stop_now == 1 || sflag == 0 
        [iteration_data, rflag] = reconcile_best_point(funs, xp, ...
           iteration_data);
        [x, funs, fval] = write_best_to_x (iteration_data);
        stepn=norm(xc-x,inf); 
        histout = [histout', [fcount, fval, npgrad, stepn, -1, x']']';
        break;
%
% Otherwise, update the history array and keep going.
%
     else
        stepn=norm(xc-x,inf); 
        histout=[histout', [fcount, fval, npgrad, stepn, iarm, x']']';
     end

%
% Update xold. It will not be the same as xc (the point for the most
% recent stencil derivative point) if you are exiting the quasi-Newton
% loop.
%
     xold=x;
   end % Newton while loop
 end % test for stencil failure for first derivative at new scale
%
% Apply the explore_function if there is one.
%
    if imfil_explore== 1
       [fcount, iteration_data] = ...
         manage_explore(explore_function,f,iteration_data,fcount,explore_data);
    end
%
%
% After the quasi-Newton iterate, I make sure that the 
% quasi-Newton point is the best I've seen. If it's not, I fix it.
% So, 'stencil_wins' is half-way on. One consequence of this is that
% I take the best point if the inner iteration fails.
%
    [iteration_data, rflag] = reconcile_best_point(funs, x, ...
           iteration_data);
    [x, funs, fval] = write_best_to_x (iteration_data);
if rflag == 1
   [mh,nh]=size(histout);
   if histout(mh,1) == fcount
      histout=histout(1:mh-1,:);
   end
   histout=[histout', [fcount, fval, npgrad, 0, 0, x']']';
end
if verbose == 1 
    [ns,fval,h,npgrad,itc,fcount]
end
end % end of loop over the scales
complete_history=iteration_data.complete_history;
function vstencil = imfil_create_stencil(options,n)
% IMFIL_CREATE_STENCIL
% Builds the stencil for imfil.m. As we evolve this we will be 
% adding the ability to do random rotations for all or part of
% a stencil and all sorts of other stuff.
%
% function vstencil = imfil_create_stencil(options,n)
%
% Input: 
%        options = imfil.m options structure
%              n = dimension
%
% Output:      
%              v = stencil
%
%
% C. T. Kelley, July 14, 2009
% This code comes with no guarantee or warranty of any kind.
%

stencil=imfil_optread('stencil',options);
vstencil=imfil_optread('vstencil',options);
if nargin ~= 2
   error('imfil_create_stencil requires two arguments');
end
%
% Is vstencil really there?
%
[mv,nv]=size(vstencil);
if mv+nv > 0
   stencil=-1;
end
%
% Build the stencil.
%
switch stencil
   case -1; % Custom stencil is ok.

   case 0 % central difference stencil
     v = [eye(n),-eye(n)];

   case 1 % one sided stencil = ON THE WAY OUT!
     v = eye(n);

   case 2 % positive basis stencil
     v = [eye(n),-ones(n,1)/sqrt(n)];

   otherwise
     error(' illegal stencil in imfil_create_stencil');
end
%
% If vstencil is a custom job, take it. Otherwise use one of
% the internal choices.
%
if stencil ~= -1
   vstencil=v;
end

function dscal = imfil_create_scales(options)
% IMFIL_CREATE_SCALES
% Creates the scales for imfil.m. Nothing much here right now, but 
% custom scales ... are in the near future.
%
% function dscal = imfil_create_scales(options)
%
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%
custom_scales=imfil_optread('custom_scales',options);
mcs=length(custom_scales);
if mcs > 0
   dscal=custom_scales;
else
   scalestart = imfil_optread('scalestart',options);
   scaledepth= imfil_optread('scaledepth',options);
%
% Do some error checking. Warn or complain as needed.
%
   if scalestart > scaledepth
      error(' imfil_create_scales: error in scales, scalestart > scaledepth');
   end
   dscal=-(scalestart:scaledepth); dscal=2.^dscal;
end
function stencil_data = ...
        create_stencil_data(options, imfil_fscale, noise_val, bounds)
% CREATE_STENCIL_DATA
%
%   Build a structure with the data, targets, errors, and deltas that
%   you need to compute the stencil derivative and  determine stencil failure.
%   This structure gets passed to manage_stencil_diff and contains everything
%   that depends only on the scale and the options. I'm doing this mostly
%   to keep the argument list from occupying several lines and confusing me.
%
% function stencil_data = ...
%      create_stencil_sdata(options, imfil_fscale, noise_val, bounds)
%
% Do not attempt to land here.
%
n = length(bounds(:,1));
v=imfil_create_stencil(options,n);
imfil_stencil_delta = imfil_optread('stencil_delta',options);
imfil_svarmin       = imfil_optread('svarmin',options);
imfil_stencil_delta = imfil_stencil_delta/imfil_fscale;
imfil_svarmin       = imfil_svarmin/imfil_fscale;
%
stencil_data=struct('stencil_delta',imfil_stencil_delta, ...
                        'svarmin',imfil_svarmin, ...
                        'noise_val', noise_val, ...
                        'bounds', bounds, 'v', v);

function [sdiff,sgrad,npgrad,fcount,sflag,jac,iteration_datap,stop_now]...
   = manage_stencil_diff(x,f,funs,iteration_data,fcount,...
        stencil_data,stop_now)
% MANAGE_STENCIL_DIFF
% This function calls stencil_diff to compute the stencil derivative,
% updates the evaluation counter and complete_history, runs through
% all the tests for stencil failure, and sends back everything
% imfil_core needs to do its job.
%
% This is an internal function. There is no reason to hack this code.
% Don't do it.
%
% This function is under development and changes frequently.
%
% C. T. Kelley, January 12, 2011
%
if stop_now == 0
%
options=iteration_data.options;
imfil_complete_history = imfil_optread('complete_history',options);
h=iteration_data.h;
%
% Unpack the stencil_data structure.
%
stencil_delta = stencil_data.stencil_delta;
svarmin = stencil_data.svarmin;
noise_val = stencil_data.noise_val;
obounds = iteration_data.obounds;
bounds = stencil_data.bounds;
v = stencil_data.v;
%
% Complete the direction matrix and compute the stencil derivative.
%
vv=imfil_augment_directions(x,v,h,options,bounds);
complete_history=iteration_data.complete_history;
[sgrad,fb,fbf,xb,icount,sflag,svar,diff_hist,jac]...
        = stencil_diff(x,f,h*vv,funs,iteration_data,complete_history);
fcount=fcount+icount;
pgrad=x - kk_proj(x-sgrad,obounds);
npgrad=norm(pgrad,inf);
%
% Run the optional stencil failure tests.
%
%
% If noise_aware = 1 and the scaled variation in f is < than the
% function's estimate of the noise, then I declare stencil failure!
%
       if max(noise_val,svarmin) > svar
           sflag=0;
       end
% If stencil_delta > 0, then I terminate the entire optimization
% when the scaled variation in f < stencil_delta. I report this as
% stencil failure as well.
%
       if stencil_delta > svar;
          stop_now=1;
          sflag=0;
       end
%
% Update the iteration_data structure.
%
iteration_datap=iteration_data;
if imfil_complete_history > 0
   complete_history = many_point_hist_update(complete_history,diff_hist);
   iteration_datap.complete_history=complete_history;
end
[iteration_datap, rflag] = reconcile_best_point(fbf, xb, ...
                      iteration_datap);
sdiff=jac_or_grad(sgrad, jac, options);
end % end of stop_now if statement
function [new_data, rflag] ...
        = reconcile_best_point(funs, x, old_data);
% RECONCILE_BEST_POINT
% function [new_data, rflag] ...
%        = reconcile_best_point(funs, x, old_data)
% 
% After the poll, or when it's time to terminate the inner or outer
% iteration, you may have a new best point.
% This function updates the record of the best point using the 
% iteration_data structure.
%
% Input: x        = current point
%        funs     = f(x)
%    old_data     = iteration_data structure    
%  least_squares  = Are we solving a nonlinear least squares problem?
%
% Output: new_data     = updated iteration_data structure    
%         fvalout = f(xout)
%         rflag = 0 if xout = x;  (ie f(x) is best, new best point)
%         rflag = 1 if xout = xb; (best point unchanged)
%
new_data=old_data;
least_squares=old_data.options.least_squares;
rflag = 1;
fb = old_data.fobjb;
fval=f_to_vals(funs,least_squares);
if fval < fb
   rflag = 0;
   new_data.xb=x;
   new_data.funsb = funs;
   new_data.fobjb = fval;
end

function [x, funs, fval] = write_best_to_x (iteration_data)
% WRITE_BEST_TO_X
% Makes the best point the current iterate.
%
% function [x, funs, fval] = write_best_to_x (iteration_data)
%
x = iteration_data.xb;
funs = iteration_data.funsb;
fval = iteration_data.fobjb;

function [sflag,best_value,best_value_f,best_point,svar,diff_hist]=...
          collect_stencil_data(good_points,good_values,failed_points,...
                  best_point_old,best_value_old,best_value_f_old,fc,options)
least_squares=imfil_optread('least_squares',options);
%
% Who's number one?
%
good_scalars=f_to_vals(good_values,least_squares);
[xbest_value,ibest]=min(good_scalars);
xbest_point=good_points(:,ibest);
xbest_value_f=good_values(:,ibest);
if xbest_value < best_value_old
   best_value = xbest_value;
   best_value_f = xbest_value_f;
   best_point = xbest_point;
else
   best_value = best_value_old;
   best_value_f = best_value_f_old;
   best_point = best_point_old;
end
%
% Find the big loser.
%
worst_value=max(good_scalars);
%
% Assemble the history structure.
%
diff_hist=struct('good_points',good_points,...
'good_values',good_values,'failed_points',failed_points);
%
% What's the total variation?
%
svar=worst_value-best_value;
%
% Stencil failure?
%
sflag=1;
if abs(best_value_old-best_value) < 1.d-14
   sflag=0;
   jac=[];
   grad=[];
end

function [xp, fval, funs, fcount, iarm, iteration_datap, nfail, hess] = ...
   imfil_inner_iteration(f, x, fx, sdiff, xc, gc, ...
             iteration_data, hess, fcount)
% IMFIL_INNER_ITERATION
% Manage the various inner iteration options.
%
% Inputs:
%         f = objective function; f returns an residual vector.
%         x = current point.
%       fun = current (vector) residual at x.
%       jac = stencil Jacobian at x.
%        xc = previous point; xc is not used in this function, but
%             that may change if we elect to put a quasi-Newton
%             method in here. xc=x for the first iteration in the 
%             inner iteration.
%        gc = stencil gradient at xc.
% iteration_data = internal structure with many goodies inside; rtfm
%   hessold = Gauss-Newton model Hessian at xc. Dummy argument
%             waiting for quasi-Newton or Levenberg-Marquardt.
%
% Output:
%      hess = Gauss-Newton model Hessian at x; no update done in here.
%             It's in the argument lists only to enable a quasi-Newton
%             update or a Levenberg-Marquardt iteration.
%        xp = new point.
%     fvalp = least squares error at xp = funs'*funs/2;
%      funs = vector residual at xp.
%      qfct = cost in function evaluations.
%      iarm = number of step size/trust region radius reductions.
% diff_hist = history data for the Gauss-Newton loop
%     nfail = 0 if the line search/trust region/LM succeeds, 1 if it fails
%
%
h = iteration_data.h;
options = iteration_data.options;
core_data = iteration_data.core_data;
imfil_least_squares=options.least_squares;
imfil_complete_history=options.complete_history;
obounds=iteration_data.obounds;
imfil_exec = options.executive;
if imfil_exec == 1
    imfil_exec_function = options.executive_function;
end
%
if imfil_exec == 0
    if imfil_least_squares == 0
         [xp, fval, funs, fct, iarm, diff_hist, nfail, hess] = ...
             imfil_qn_update(f, x, fx, sdiff, xc, gc, ...
             iteration_data, hess);
    else
         [xp, fval, funs, fct, iarm, diff_hist, nfail, hess] ...
               = imfil_gauss_newton(f, x, fx, sdiff, xc, gc, ...
                 iteration_data, hess);
    end
else
         [xp, fval, funs, fct, iarm, diff_hist, nfail, hess] ...
               = feval(imfil_exec_function,f, x, fx, sdiff, xc, gc, ...
                 iteration_data, hess);
end
fcount=fcount+fct;
iteration_datap=iteration_data;
if imfil_complete_history > 0
   iteration_datap.complete_history...
        = many_point_hist_update(iteration_data.complete_history,diff_hist);
end
[iteration_datap, rflag] = reconcile_best_point(funs, xp, iteration_datap);
function [fct,x,fval,iarm,fres,diff_hist, nfail]=...
  armijo_explore(f, sdir, fold, xc, h, core_data, obounds)
% ARMIJO_EXPLORE
% Line search for imfil.m.
%
% C. T. Kelley, September 15, 2008
%
% This code comes with no guarantee or warranty of any kind.
%
% function [fct,x,fval,iarm,fres,diff_hist, nfail]=...
%       armijo_explore(f, sdir, fold, xc, h, core_data, obounds)
%
% This is an internal function, which you are NOT TO HACK! Since
% you may hack it anyway, I will tell you want is going on. If you
% break something, may Alberich's curse be upon you!
%
% Inputs: f = objective function.
%         sdir = quasi-Newton search direction.
%
%         fold = current function value.
%
%         maxitarm = limit on number of step size reductions.
%
%         beta = stepsize reduction factor.
%
%         h = current scale.
%
% core_data = structure with the options + fun_data
%              The options are documented in optset.m.
%              fun_data is private to imfil.m and is used in the internal
%              scaling for the function evaluation.
%
%         obounds = Nx2 array of scaled bound constraints from imfil_core.
%
% Output: fct = cost of line search in function evaluations.
%
%         x = new point.
% 
%         fval = f(x).
%  
%         iarm = number of step length reductions.  
%
%         fres = residual for nonlinear least squares problems.
%
%    diff_hist = history data for this iteration.
%
%        nfail = 0 if the line search succeeds, 1 if it fails.
%
% Read the options array to get the ones we care about in the line search.
%
options = core_data.options;
imfil_parallel=imfil_optread('parallel',options);
imfil_least_squares=imfil_optread('least_squares',options);
imfil_verbose=imfil_optread('verbose',options);
beta = imfil_optread('armijo_reduction',options);
maxitarm = imfil_optread('maxitarm',options);
%
if imfil_parallel == 0
[fct,x,fval,iarm,aflag,fres,diff_hist, nfail]=...
  serial_armijo(f, sdir, fold, xc, h, obounds, core_data);
else
[fct,x,fval,iarm,aflag,fres,diff_hist, nfail]=...
  parallel_armijo(f, sdir, fold, xc, h, obounds, core_data);
end
%
if iarm == maxitarm & aflag == 1 & imfil_verbose == 1
       disp(' line search failure'); [iarm, h]
end
function [fct,x,fval,iarm,aflag,fres,diff_hist,nfail]=...
  parallel_armijo(f, sdir, fold, xc, h, obounds, core_data)
% PARALLEL_ARMIJO
% Parallel line search for imfil.m.
% Uses extra processors to explore larger stencils.
%
% function [fct,x,fval,iarm,aflag,fres]=...
%    parallel_armijo(f, sdir, fold, xc, h, obounds, core_data)
%
% Before you modify this (to use a trust region method, say), consider
% writing your own with this as a template. I'll make this very easy
% in a later version of the code. 
%
% Inputs:
%          f = objective function or residual vector, depending
%              on whether this is a least squares problem or not.
%       sdir = search direction.
%       fold = current value of the objective.
%       xc   = current point.
%   maxitarm = limit on number of step size reductions.
%       beta = reduction factor of step size. Currently beta = 1/2.
%          h = current scale.
%     obounds = Nx2 array of scaled bound constraints from imfil_core.
%
%  core_data = structure with the options + fun_data
%              The options are documented in optset.m.
%              fun_data is private to imfil.m and is used in the internal
%              scaling for the function evaluation.
% imfil_least_squares = least squares flag.
%
% Output:
%        fct = cost in function evaluations.
%          x = new point.
%       fval = objective function value at new point = fres'*fres/2
%              in the nonlinear least squares case.
%      iarm  = number of step size reductions.
%      aflag = 0 if the search finds a new point, = 1 if not.
%      fres  = residual for nonlinear least squares problems
%  diff_hist = history structure for this iteration
%      nfail = 0 if the line search succeeds, 1 if it fails.
%
%
% C. T. Kelley, July 21, 2009
% This code comes with no guarantee or warranty of any kind.
%
options = core_data.options;
imfil_least_squares=imfil_optread('least_squares',options);
imfil_limit_quasi_newton=imfil_optread('limit_quasi_newton',options);
imfil_verbose=imfil_optread('verbose',options);
beta = imfil_optread('armijo_reduction',options);
maxitarm = imfil_optread('maxitarm',options);
%
% Initialize the line search.
%
lambda=1;
n=length(xc);
fct=0;
aflag=1;
dd=sdir;
fres=[];
frest=fres;
diff_hist=struct('good_points',[],'good_values',[],'failed_points',[]);
%
if imfil_limit_quasi_newton == 1
%
% If the quasi-Newton step is much longer than the scale, shrink it.
%
% I am not convinced that this is the right thing to do, but it
% really helps most of the time.
%
 smax=10*min(h,1); if norm(dd) > smax dd=smax*dd/norm(dd); end
%
end
x=xc;
fval=fold;
%
% Evaluate all steplength choices at once.
%
number_steps=maxitarm+1;
ddm=zeros(n,number_steps);
for i=1:number_steps
   ddm(:,i)=xc-lambda*dd;
   lambda=beta*lambda;
   ddm(:,i)=kk_proj(ddm(:,i),obounds);
end
[fta,iflaga,ictra]=feval(f,ddm,h,core_data);
%
% Put the failed points in the history structure.
%
ibad=(iflaga(1:number_steps)==1);
if sum(ibad) > 0
   diff_hist.failed_points=ddm(:,ibad);
end
%
% Get the good points for the history structure.
%
igood=(iflaga(1:number_steps)==0);
if sum(igood) > 0
    diff_hist.good_points=ddm(:,igood);
    if imfil_least_squares == 1
       diff_hist.good_values=fta(:,igood);
    else
       diff_hist.good_values=fta(:,igood);
%       diff_hist.good_values=fta(igood);
    end
end
%
if imfil_least_squares == 1
     frest=fta;
end
fta=f_to_vals(fta,imfil_least_squares);
fct=fct+sum(ictra);
ilose=(iflaga==1);
fta(ilose) = fold+abs(fold)*.0001+1.d-12;
%
% Traditional duplicates a serial Armijo search. This will take
% the longest possible step and is sometimes the right thing to do.
%
% Non-traditional will take the best point from all the
% ones sampled in the search direction if the full step fails.
%
% I am still playing with this, and will let you chose in the options
% command pretty soon. 
%
traditional = 0;
if traditional == 0
%
[ft,it]=min(fta); xt=ddm(:,it);
iarm=maxitarm;
if ft < fval 
    aflag=0; fval=ft;
    if imfil_least_squares == 1
       fres=frest(:,it); 
    end
    x=xt; iarm=min(it,maxitarm-1); 
end
%
else
%
iarm=-1;
%
while iarm < maxitarm & aflag==1 
    ft=fta(iarm+2);
    xt=ddm(:,iarm+2);
    if ft < fval & aflag==1; aflag=0; 
            if imfil_least_squares == 1
                  fres=frest(:,iarm+2); 
            end
       fval=ft; x=xt; end
    iarm=iarm+1;
end
end
if iarm == maxitarm && aflag == 1 && imfil_verbose == 1
       disp(' line search failure'); [iarm, h]
end
nfail=aflag;
function [fct,x,fval,iarm,aflag,fres,diff_hist,nfail]=...
  serial_armijo(f, sdir, fold, xc, h, obounds, core_data);
% SERIAL_ARMIJO
% Serial line search for imfil.m.
%
% function [fct,x,fval,iarm,aflag,fres]=...
%   serial_armijo(f, sdir, fold, xc, h, obounds, core_data)
%
% This is a plain vanilla serial line search. Nothing to see here;
% move along.
%
% Before you modify this (to use a trust region method, say), consider
% writing your own with this as a template. I'll make this very easy
% in a later version of the code.
%
% Inputs:
%           f = objective function or residual vector, depending
%              on whether this is a least squares problem or not.
%        sdir = search direction.
%        fold = current value of the objective.
%        xc   = current point.
%           h = current scale.
%     obounds = Nx2 array of scaled bound constraints from imfil_core.
%
%   core_data = structure with the options + fun_data
%              The options are documented in optset.m.
%              fun_data is private to imfil.m and is used in the internal
%              scaling for the function evaluation.
%
% Output:
%        fct = cost in function evaluations.
%          x = new point.
%       fval = objective function value at new point = fres'*fres/2
%              in the nonlinear least squares case.
%      iarm  = number of step size reductions.
%      aflag = 0 if the search finds a new point, = 1 if not.
%      fres  = residual for nonlinear least squares problems
%  diff_hist = history structure for this iteration
%     nfail = 0 if the line search succeeds, 1 if it fails.
%
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%
options = core_data.options;
imfil_least_squares=imfil_optread('least_squares',options);
imfil_limit_quasi_newton=imfil_optread('limit_quasi_newton',options);
imfil_verbose=imfil_optread('verbose',options);
beta = imfil_optread('armijo_reduction',options);
maxitarm = imfil_optread('maxitarm',options);
%
% Initialize the line search.
%
lambda=1;
n=length(xc);
iarm=-1;
fct=0;
aflag=1;
dd=sdir;
fres=[];
frest=fres;
diff_hist=struct('good_points',[],'good_values',[],'failed_points',[]);
%
if imfil_limit_quasi_newton == 1
%
% If the quasi-Newton step is much longer than the scale, shrink it.
%
% I am not convinced that this is the right thing to do, but it
% really helps most of the time.
%
 smax=10*min(h,1); if norm(dd) > smax dd=smax*dd/norm(dd); end
%
%
end
x=xc;
fval=fold;
while iarm < maxitarm & aflag==1 
        d=-lambda*dd;
        xt=x+d;
        xt=kk_proj(xt,obounds);
        [ft,ifl,ict]=feval(f,xt,h,core_data); 
        fct=fct+ict;
        diff_hist=single_point_hist_update(diff_hist,xt,ft,ifl);
        if imfil_least_squares == 1
           frest=ft;
           ft=frest'*frest/2;
        end
        if ifl==1
           ft=fold+abs(fold)*.0001+1.d-12;
        end
        if ft < fval & aflag==1; aflag=0; fval=ft; 
                                 fres=frest; x=xt; 
        end
        if aflag==1; lambda=beta*lambda; end
    iarm=iarm+1;
end
if iarm == maxitarm & aflag == 1 & imfil_verbose == 1
       disp(' line search failure'); [iarm, h]
end
nfail = aflag;
function [xp, fvalp, funs, qfct, iarm, diff_hist, nfail, hess] ...
         = imfil_gauss_newton(f, x, fun, jac, xc,  gc, ...
                iteration_data, hessold)
% IMFIL_GAUSS_NEWTON
% Compute a damped Gauss-Newton step.
%
% function [xp, fvalp, funs, qfct, iarm, hess] ...
%         = imfil_gauss_newton(f, x, fun, jac, xc, gc, ...
%                iteration_data, hessold)
%
% You may elect to modify this routine if you don't want to spend the
% effort computing the Gauss-Newton model Hessian. The current version
% of imfil.m does not use it.
%
% Inputs: 
%         f = objective function; f returns an residual vector.
%         x = current point.
%       fun = current (vector) residual at x.
%       jac = stencil Jacobian at x.
%        xc = previous point; xc is not used in this function, but
%             that may change if we elect to put a quasi-Newton 
%             method in here.
%        gc = stencil gradient at xc.
% iteration_data = internal structure with many goodies inside; rtfm
%   hessold = Gauss-Newton model Hessian at xc. Dummy argument
%             waiting for quasi-Newton or Levenberg-Marquardt.
%
% Output:
%      hess = Gauss-Newton model Hessian at x; no update done in here.
%             It's in the argument lists only to enable a quasi-Newton
%             update or a Levenberg-Marquardt iteration.
%        xp = new point.
%     fvalp = least squares error at xp = funs'*funs/2;
%      funs = vector residual at xp.
%      qfct = cost in function evaluations.
%      iarm = number of step size reductions.
% diff_hist = history data for the Gauss-Newton loop
%     nfail = 0 if the line search succeeds, 1 if it fails
%        
%
% C. T. Kelley, January 10, 2011
% This code comes with no guarantee or warranty of any kind.
%
% Harvest what you need from iteration_data.
%
obounds=iteration_data.obounds;
options=iteration_data.options;
core_data=iteration_data.core_data;
h=iteration_data.h;
%
% Compute stencil gradient (sgrad) and least squares error (fval)
% at current point.
%
funs=fun;
fval=fun'*fun/2;
sgrad=jac'*fun;
%
hess=jac'*jac; % Return the normal equations model Hessian for now
%
imfil_verbose=imfil_optread('verbose',options);
n=length(x);
%
% Get the epsilon-active indices and encode them in a diagonal matrix.
%
epsb=1.d-6;
alist = max((x > obounds(:,1)+epsb) , (x < obounds(:,2)-epsb));
pr=diag(alist);
%
% Compute the search direction with a QR factorization.
%
rjac=jac*pr;
[rq,rr]=qr(rjac);
sdir1=(eye(n)-pr)*sgrad;
sdir2=rq'*fun;
sdir2=rr\sdir2;
sdir=sdir1+sdir2;
%
% Bound constrained line search
%
[qfct,xp,fvalp,iarm,fres,diff_hist, nfail]=armijo_explore(f, sdir, fval, x, ...
    h, core_data, obounds);
%
% If the line search fails nothing changes.
%
if length(fres) > 0
   funs=fres;
end
function [xp, fvalp, funs, qfct, iarm, diff_hist, nfail, hess] ...
           = imfil_qn_update(f, x, fval, sgrad, xc, gc, iteration_data,...
             hessold)
% IMFIL_QN_UPDATE
% function [hess, xp, fvalp, funs, qfct, iarm, diff_hist] ...
%           = imfil_qn_update(f, x, fval, sgrad, xc, gc, iteration_data,...
%             hessold)
% 
% Quasi-Newton update of point and Hessian. This function is never called
% for least squares problems.
%
% Input: 
%        f = objective function.
%        x = current point.
%     fval = f(x). 
%    sgrad = stencil gradient at current point x.
%       xc = previous point.
%       gc = stencil gradient at previous point xc.
% iteration_data = imfil internal structure
%  hessold = previous model Hessian
%
% Output:
%     hess = Quasi-Newton update Hessian at x.
%       xp = new point.
%    fvalp = f(xp)
%    funs  = fvalp; Makes calling sequence compatible with Gauss-Newton
%     qfct = cost in function evaluations.
%     iarm = number of step size reductions.
% diff_hist= history data for the quasi-Newton loop
%     nfail= 0 if line search succeeds, 1 if it fails
%
% 
% The steps are (1) update Hessian to obtain hess(current Hessian),
%               (2) use hess, gc, and the bounds to compute the new
%                   search direction,
%               (3) do a line search on that direction,
%
% C. T. Kelley, January 23, 2011
% This code comes with no guarantee or warranty of any kind.
%
core_data=iteration_data.core_data;
%
% obounds are the scaled 0-1 bounds. I use this field because my quasi-Newton
% codes were written for general bounds and I do not want to invade them
% any more than necessary.
%
obounds=iteration_data.obounds;
h=iteration_data.h;
%
% itc is the inner iteration counter. I am not updating the model Hessian
% for the first inner iteration. The reason for this is that I need a 
% couple gradients at each scale to make the quasi-Newton formula make sense.
%
itc=iteration_data.itc;
options=core_data.options;
imfil_verbose=imfil_optread('verbose',options);
quasi = imfil_optread('quasi',options);
%
nx=length(x);
hess=hessold;
%
% Update the model Hessian for all but the initial inner iteration.
%
if itc > 1
%
% Get the epsilon-inactive indices.
%
epsb=1.d-6;
alist = (x > obounds(:,1)+epsb) & (x < obounds(:,2)-epsb);
%
switch quasi
   case 1 % BFGS
      hess = bfupdate(x, xc, sgrad, gc, hessold, alist);
   case 2 % SR1
      hess = sr1up(x, xc, sgrad, gc, hessold, alist);
   otherwise % nothing
       hess = eye(nx,nx);
end
end
%
% Search direction
%
sdir = hess\sgrad;
%
% Bound constrained line search
%
[qfct,xp,fvalp,iarm,fres,diff_hist,nfail]=armijo_explore(f, sdir, fval, x, ...
    h, core_data, obounds);
funs=fvalp;

%
%   BFGS update of reduced Hessian, nothing fancy.
%
function hess = bfupdate(x, xc, sgrad, gc, hess,alist)
n=length(x);
pr=diag(alist);
y=sgrad-gc; s=x-xc; z=hess*s;
%
% Turn y into y#.
%
y=pr*y;
if y'*s > 0
   hess = pr*hess*pr + (y*y'/(y'*s)) - pr*(z*z'/(s'*z))*pr;
   hess=eye(n) - pr + hess;
end
if cond(hess) > 1.d6
    hess = eye(n);
end

%
% SR1 update of reduced Hessian.
%
function hess = sr1up(x, xc, sgrad, gc, hess,alist)
n=length(x);
pr=diag(alist);
y=sgrad-gc; s=x-xc; 
z=y - hess*s;
%
% Turn y into y#.
%
y=pr*y; z=pr*z;
if z'*s ~=0
        ptst = z'*(hess*z)+(z'*z)*(z'*z)/(z'*s);
        if ptst > 0 
            hess = pr*hess*pr + (z*z')/(z'*s); 
            hess = eye(n) - pr + hess;
        end
end
if cond(hess) > 1.d6
    hess = eye(n);
end
function [grad,best_value,best_value_f,best_point,icount,...
            sflag,svar,diff_hist,jac]=...
               stencil_diff(x,f,dx,fc,iteration_data,complete_history)
% STENCIL_DIFF 
% Stencil derivative for optimization and nonlinear least squares. 
% This function also tests for best point in stencil.
%
% function [grad,best_value,best_value_f,best_point,icount,...
%            sflag,svar,diff_hist,jac]=...
%              stencil_diff(x,f,dx,fc,iteration_data,options,h,complete_history)
%
%   Input:  x  = current point
%           f  = objective function (or vector residual for
%                nonlinear least squares)
%           dx = scaled difference directions
%           fc = current function value
%
%   iteration_data = structure with the options + fun_data +
%                    current iteration parameters.
%               The options are documented in optset.m.
%               fun_data is private to imfil.m and is used in the internal
%               scaling for the function evaluation.
%
% complete_history = every point imfil has ever seen. That data are used
%                    here to prevent redundant evaluations.
%
%   Output: 
%         grad = stencil gradient; NaN if every point on the stencil is bad
%  best_value  = best value in stencil
%  best_value_f= best least squares residual in stencil
%  best_point  = best point in stencil
%       icount = counter of calls to expensive part of function
%                 the function needs to return this.
%        sflag = 0 current point is best in the stencil
%                     This means stencil failure.
%
%              = 1 means there's a better point on the stecil.
%
%         svar = variation on stencil = max - min
%      
%    diff_hist = every function evaluation for points satisfying the 
%                 bounds stored in a nice struct. This is used to update
%                 the complete_history structure.
%
%          jac = stencil Jacobian for least squares problems.
%
% C. T. Kelley, July 30, 2009
% This code comes with no guarantee or warranty of any kind.
%
core_data=iteration_data.core_data;
h=iteration_data.h;
%
% bounds = iteration_data.obounds are the 0-1 bounds for the 
% scaled problem.
%
bounds=iteration_data.obounds;
%
options = core_data.options;
parallel=imfil_optread('parallel',options);
least_squares=imfil_optread('least_squares',options);
%
% Poll the stencil and collect some data.
%
[best_value,best_value_f,best_point,icount,sgood,good_points,good_values,...
         good_dx,good_df,failed_points] = ...
           imfil_poll_stencil(x,f,dx,fc,bounds,core_data,h,complete_history);
%
% Initialize the search for best value at the center.
%
if least_squares == 1
fval = fc'*fc/2; % best value in the stencil
    else
fval = fc;
end
%
% Use the points and values to get the stencil statisitcs.
%
[sflag,best_value,best_value_f,best_point,svar,diff_hist]=...
    collect_stencil_data(good_points,good_values,failed_points,...
                x,fval,fc,fc,options);
%
% sgood = 0 means there are no good points. Shrink time!
%
% Estimate the derivative.
% The idea is that fprime*dx approx df, where
% fprime   = m x n is the gradient-transpose or the Jacobian
% good_dx  = n x pnew (pnew <= vsize) is the matrix of good steps
% good_df  = m x pnew is the collection of good function values at x + dx
% 
% So the least squares problem to be solved is 
%         min || fprime * good_dx - good_df ||
% for the columns of fprime. The minimum norm solution is
% fprime = good_df*pinv(good_dx).
%
if sgood > 0
   pdx=pinv(good_dx);
   fprime=good_df*pdx;
   if least_squares == 0
      grad=fprime';
      jac=[];
   else
      jac=fprime;
      grad=fprime'*fc;
   end
else
   grad=NaN*x;
   jac=[];
end
function px = kk_proj(x,bounds)
% KK_PROJ
% Projection onto the feasible hyperrectangle.
%
% function px = kk_proj(x,bounds)
%
% Not exciting stuff.
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%
ndim=length(x);
px=zeros(ndim,1);
px=min(bounds(:,2),x);
px=max(bounds(:,1),px);
function fvals=f_to_vals(funs,least_squares)
%
% F_TO_VALS
% Evaluate fvals = f^T f/2 when f is a vector least squares residual.
%
% function fvals=f_to_vals(funs,least_squares)
%
% There is no reason you'd want to mess with this.
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%
[m,n]=size(funs);
if m == 0 && n == 0
   fvals=[];
end
fvals=zeros(n,1);
if least_squares == 1
  for i=1:n
    fvals(i)=funs(:,i)'*funs(:,i)/2;
  end
else
    fvals=funs;
end

function vout=imfil_augment_directions(x,vin,h,options,bounds)
% IMFIL_AUGMENT_DIRECTIONS
% Enrich the stencil with random directions and a user-supplied
% new_direcions function.
%
% C. T. Kelley, August 13, 2009.
% This code comes with no guarantee or warranty of any kind.
%
% Add the random directions.
%
random_stencil = imfil_optread('random_stencil',options);
vout= random_augment(vin,random_stencil);
%
% See if there's a new_directions function.
%
new_directions = imfil_optread('add_new_directions',options);
lnew=length(new_directions);
if lnew > 0
    dbv=bounds(:,2)-bounds(:,1); db=diag(dbv);
    unscaled_x = db*x + bounds(:,1);
%    unscaled_v = db*vin;
    unscaled_v = db*vout;
    unscaled_vnew=feval(new_directions, unscaled_x, h, unscaled_v);
    [mv,nv]=size(unscaled_vnew);
    if mv > 0
        vnew=inv(db)*unscaled_vnew;
        [mv,nv]=size(vnew);
        for i=1:nv
           vnew(:,i)=vnew(:,i)/norm(vnew(:,i),inf);
        end
        vout=[vout, vnew];
    end
end

function vout=random_augment(vin,k)
% RANDOM_AUGMENT
% Add k random unit vectors to the stencil.
%
% This makes some of the theory work, but it is not always
% a good idea to do this. Adding more vectors makes stencil failure
% less likely, which is not good if you have a full stencil and you're
% wasting lots of time in the line search.
%
% Adding random vectors makes more sense if the minimizer is 
% on a hidden or explicit constraint boundary.
%
% C. T. Kelley, January 5, 2011
% This code comes with no guarantee or warranty of any kind.
%
[n,nn]=size(vin);
if k==0
   vout=vin;
end
%
% Generate k uniformly distributed points on the unit sphere.
% See Marsaglia, G. "Choosing a Point from the Surface of a Sphere." 
%     Ann. Math. Stat. 43, 645-646, 1972.
%
rv=randn(n,k);
for ir=1:k
   rv(:,ir)=rv(:,ir)/norm(rv(:,ir));
end
vout=[vin,rv];

function [best_value,best_value_f,best_point,icount,sgood,...
      good_points,good_values,good_dx,good_df,failed_points] = ...
              imfil_poll_stencil(x,f,dx,fc,bounds,core_data,h,complete_history)
% IMFIL_POLL_STENCIL
% Poll the stencil and organize the results.
% I do not think you will want to modify this code.
%
% function [best_value,best_point,icount,sgood,good_points,good_values,...
%           good_dx,good_df,failed_points] = ...
%                          imfil_poll_stencil(x,f,dx,fc,bounds,options,h)
%
% Input:
%        x = center of stencil
%        f = objective function
%       dx = h*V = array of scaled directions
%       fc = f(x)
%   bounds = N x 2 array of lower/upper bounds
%  core_data = imfil core_data structure
%        h = current scale
%
% Output:
%
% The best_point data is frozen at xc for this function. We only keep
% track of it here so we can ship it to 
%     best_point    = f(best_point) == best_value
%     best_value    = lowest value of f on the stencil
%     best_value_f  = best least squares residual
%     icount        = cost in units of calls to f
%     sgood         = number of successful evaluations
%     good_points   = list of points at which f returned a value
%     good_values   = f(good_points)
%     good dx       = vector of good_points - x
%     good df       = vector of f(good_points) - fc
%     failed_points = list of points at which f did not return a value
%
% C. T. Kelley, Aug 13, 2009
% This code comes with no guarantee or warranty of any kind.
%
options=core_data.options;
parallel=imfil_optread('parallel',options);
least_squares=imfil_optread('least_squares',options);
[n,vsize]=size(dx);
%
% Record the best point and best function value in the stencil.
% Get started by using the center point.
%
best_point=x;
if least_squares == 1
best_value=fc'*fc/2; % best value in the stencil
    else
best_value=fc;
end
best_value_f=fc;
%
iflag=zeros(vsize,1);
m=length(fc); % m > 1 tells me it's a least squares problem
fp=zeros(m,vsize);
failed_points=[];
good_points=[];
good_values=[];
sgood=0;
fval=zeros(vsize,1);
icount=0;
%
% fp(:,i) and fval(i) are not defined outside of the bounds
% or if iflag(i)=1.
%
% First cull the points which violate the bound constraints.
%
% Collect the feasible points, differences, and functions in xp1 and dx1.
%
%
pold=0;
dx1=[];
xp1=[];
%
% One-sided differences may need to flip the direction if the positive
% perturbation is not feasible.
%
stencil_type=options.stencil;
if stencil_type == 1
   for i=1:vsize
     xp(:,i)=x+dx(:,i);
     if isok(xp(:,i),bounds) == 0
        dx(:,i)=-dx(:,i);
     end
   end
end
%
%
%
for i=1:vsize
  xp(:,i)=x+dx(:,i);
  if isok(xp(:,i),bounds)
     pold=pold+1;
     dx1=[dx1,dx(:,i)];
     xp1=[xp1,xp(:,i)];
  end
end
fp1=fp(:,1:pold);
xp=xp1; dx=dx1; fp=fp1;
%
% Query the complete_history structure to see if you've evaluated f
% at any of these points before.
%
[oldindex,oldpoints,oldvalues,oldflags]= ...
               scan_history(complete_history,xp,fp,dx);
newindex=~oldindex;
xp=xp1(:,newindex);
iflago=zeros(1,vsize);
if sum(oldindex) > 0
   fp(:,oldindex)=oldvalues;
   iflago(oldindex)=oldflags;
end
%xp=xpout; dx=dxout; fp=fpout;
%
% Evaluate f, in parallel if possible. Flag the failed points.
%
pnew=sum(newindex);
fp1=[];
iflag=[];
if parallel == 0
    for i=1:pnew
        [fpx,iflagx,ict]=feval(f,xp(:,i),h,core_data);
        fp1=[fp1,fpx];
        iflag=[iflag,iflagx];
        icount=icount+ict;
    end
else
    if pnew > 0
        [fp1,iflag,ictrp]=feval(f,xp,h,core_data);
        icount=icount+sum(ictrp);
    end
end
if pnew > 0
   fp(:,newindex)=fp1;
   iflago(newindex)=iflag;
end
fp1=fp;
iflag=iflago;
%
% Identify the failed points.
%
ibad=(iflag(1:pold)==1);
if sum(ibad) > 0
   failed_points=xp1(:,ibad);
end
%
% Store the good points and their function values.
%
igood=(iflag(1:pold)==0);
sgood=sum(igood);
good_dx=[];
good_df=[];
if sgood > 0
   good_points=xp1(:,igood);
   if least_squares == 1
      good_fp=fp1(:,igood);
   else
      good_fp=fp1(igood);
   end
   good_dx=dx1(:,igood);
   for ig=1:sgood
      if least_squares == 1
         good_df(:,ig)=good_fp(:,ig)-fc;
      else
         good_df(ig)=good_fp(ig)-fc;
      end
   end
%   good_values=f_to_vals(good_fp,least_squares);
   good_values=good_fp;
end

%
% test x for feasibility re bounds
%
function lisok=isok(x,bounds)
il=min(x >= bounds(:,1));
iu=min(x <= bounds(:,2));
isok=min(il,iu);
lisok = (isok == 1);

function sdiff = jac_or_grad(sgrad, jac, options)
% JAC_OR_GRAD
% returns either the simplex gradient or Jacobian depending
% on the type of problem (optization or least squares)
%
% function sdiff = jac_or_grad(sgrad, jac, options)
%
imfil_least_squares=options.least_squares;
switch imfil_least_squares
   case 1
     sdiff=jac;
   case 0 
     sdiff=sgrad;
   otherwise
     disp(' error in jac_or_grad ')
end
function [fctout, iteration_datap] = ...
         manage_explore(explore_function,f,iteration_data,fctin,explore_data);
% MANAGE_EXPLORE
% Call the user-defined explore function and update the counter
% and the iteration_data structure.
%
iteration_datap=iteration_data;
options=iteration_data.options;
imfil_complete_history=imfil_optread('complete_history',options);
%
[xs, fs, my_cost, explore_hist] = ...
        feval(explore_function,f,iteration_data,explore_data);
fctout=fctin + my_cost;
%
if imfil_complete_history > 0
   iteration_datap.complete_history...
      = many_point_hist_update(iteration_datap.complete_history,explore_hist);
end
%
[iteration_datap, rflag] = reconcile_best_point(fs, xs, iteration_datap);
function new_hist=many_point_hist_update(old_hist,diff_hist)
% MANY_POINT_HIST_UPDATE
% function new_hist=many_point_hist_update(old_hist,diff_hist)
%
% Update the complete_history structure after a many calls to f.
%
%
% WARNING! The complete_history structure uses your bounds, and is
% not scaled to make 0 <= x(i) <= 1.
%
new_hist=old_hist;
new_hist.good_points=[new_hist.good_points,diff_hist.good_points];
new_hist.good_values=[new_hist.good_values,diff_hist.good_values];
new_hist.failed_points=[new_hist.failed_points,diff_hist.failed_points];
function new_hist=single_point_hist_update(old_hist,x,fout,ifail)
%SINGLE_POINT_HIST_UPDATE
% Update the complete_history structure after a single call to f.
%
% function new_hist=single_point_hist_update(old_hist,x,fout,ifail)
%
%
new_hist=old_hist;
%
% Write the data.
%
if ifail == 1
    new_hist.failed_points=[old_hist.failed_points,x];
else
    new_hist.good_points=[old_hist.good_points,x];
    new_hist.good_values=[old_hist.good_values,fout];
end
function [oldindex,oldpoints,oldvalues,oldflags] = ...
               scan_history(complete_history,xp,fp,dx)
[mp,np]=size(xp);
imold=zeros(1,np);
oldpoints=[];
oldvalues=[];
oldflags=[];
for i=1:np
    [fpt,ift]=scal_complete_history(complete_history,xp(:,i));
    if ift > -1
       oldpoints=[oldpoints,xp(:,i)];
       oldvalues=[oldvalues,fpt];
       oldflags=[oldflags,ift];
       imold(i)=1;
    end
end
oldindex=(imold==1);


function [fhist,iflaghist]=scal_complete_history(complete_history,x)
%fhist=[];
iflaghist=-1;
losers=complete_history.failed_points;
[ml,nl]=size(losers);
winners=complete_history.good_points;
[mw,nw]=size(winners);
fhist=NaN(mw,1);
iquit=0;
if nw > 0
for i=1:nw
  d=norm(x-winners(:,i),inf);
  if d < 1.d-12
     iquit=1;
     fhist=complete_history.good_values(:,i);
     iflaghist=0;
     break;
  end
end
end
if iquit == 0 & nl > 0
for i=1:nl
   d=norm(x-losers(:,i),inf);
   if d < 1.d-12
     fhist=NaN;
     iquit=1;
     iflaghist=1;
     break;
  end
end
end
function [explore_function, explore_data_flag, explore_data] = ...
        setup_explore(options)
% SETUP_EXPLORE
% Gets the explore function organzied if you have one.
%
imfil_explore= imfil_optread('explore',options);
explore_function=[];
explore_data_flag=[];
explore_data=[];
if imfil_explore== 1
   explore_function=imfil_optread('explore_function',options);
   explore_data_flag=imfil_optread('explore_data_flag',options);
   if explore_data_flag == 1
      explore_data = imfil_optread('explore_data',options);
   end
end
function valout=imfil_optread(str,optin)
% IMFIL_OPTREAD
%
% Reads the options one at a time. This is an internal
% function for imfil.m. 
%
% Input: 
%      str = name of option
%    optin = options structure
%
% Output: 
%   valout = value of option
%
% Example: fscale=imfil_optread('fscale',optin);
%
% The options array is fully documented in the comment lines to
% imfil_optset. Type `help imfil_optset' for the details.
%
% There is no error checking in imfil_optread. 
% 
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%

str=lower(str);
if nargin <= 2
   n=-1;
end
parms=fieldnames(optin);
nopt=length(parms);
did_ok=0;
for p=1:nopt
    if strcmp(str,parms(p));
       valout=getfield(optin,str);
       did_ok=1;
    end
end

if did_ok == 0;
   error('Error in imfil_optread');
end

function eflag=imfil_error_check(varargin)
% IMFIL_ERROR_CHECK
%
% Checks dimensions and options for input errors or inconsistencies.
% This function is not intended for use outside of imfil.m. It does
% not error check itself.
%
% C. T. Kelley, Feb 19, 2010
%
str=varargin{1};
switch str
   case 'bounds'
       x=varargin{2};
       bounds=varargin{3};
       eflag=imfil_check_bounds(x,bounds);
   case 'first_eval'
       funs=varargin{2};
       options=varargin{3};
       iflag=varargin{4};
       eflag=imfil_check_first_eval(funs,options,iflag);
   otherwise
       emsg=strcat('"',str,'"',' is an illegal argument to error_check.');
       disp(emsg);
end
%
function lsqerr = imfil_check_first_eval(funs,options,iflag)
% IMFIL_CHECK_FIRST_EVAL
%
% Complain if you have a scalar-valued function + nonlinear least squares.
% Give up if you have a vector-valued function + not a least squares problem.
% Warn if the function fails to return a value at the initial iterate.
%
lsqerr=0;
[mf,nf]=size(funs);
lsq=options.least_squares;
if mf > 1 && lsq == 0
mf
 disp('Your function is vector-valued but the least_squares option is off.'); 
 disp('imfil cannot run with this inconsistency.');
 lsqerr=1;
end
if mf == 1 && lsq == 1
 disp('Your function is scalar-valued but the least_squares option is on.'); 
 disp('Are you sure this is what you want to do?');
end
if iflag > 0
 disp('Your function failed to return a value at the initial iterate.');
 disp('This is usually a problem. You have been warned.');
end
%
function badargs = imfil_check_bounds(x,bounds);
% IMFIL_CHECK_BOUNDS
%
% Sanity check for the bounds. Complain if the bounds are not the 
% same length as x, if the bounds are not properly arranged into 
% columns, if the lower bound is not less than the upper bound,
% or if x is infeasible.
%
badargs=0;
[mx,nx]=size(x);
[mb,nb]=size(bounds);
if mb ~= mx
   badargs=1;
   disp(' The columns [lower, upper] of the bounds array must have');
   disp(' same length as x. ');
end
if nb ~= 2
   badargs=1;
   disp(' The bounds array must have two columns [lower, upper].');
end
diff_vec=bounds(:,2) - bounds(:,1);
m_vec=min(diff_vec);
if m_vec <= 0
   badargs=1;
   disp(' The lower bound must be strictly greater than the upper bound.');
end
if m_vec > 0
   px=max(bounds(:,1),min(x,bounds(:,2))); 
   nd=norm(x-px);
   if nd > 0
      badargs=1;
      disp(' The initial iterate is infeasible. x must satisfy the bound constraints.');
   end
end
function [x,histout,complete_history,ifailed] = ...
                 imfil_core(x0,f,budget,core_data,bounds)
% IFFCO_CORE v1.01
% Core code for imfil.m
% Minimization of f(x) subject to explict bound constraints
%
%
% function [x, histout,complete_history] ...
%                    = imfil_core(x0,f,budget,core_data,bounds)
%
% Bound constrained, parallel, implicit filtering code.
% 
% IMPLICIT FILTERING with SR1 and BFGS quasi-Newton methods
%
% Input: 
%       x0 = initial iterate
%        f = objective function,
%
%            the calling sequence for f should be 
%            [fout,ifail,icount]=f(x,h)
%
%            h is an optional argument for f, and should be used only if your
%            function is scale-aware, ie does something useful with 
%            the scale, such as tuning accuracy.
%
%  budget = upper limit on function evaluations. The optimization
%                 will terminate soon after the function evaluation counter
%                 exceeds the budget.
%
%  core_data = structure with the options + fun_data
%              The options are documented in optset.m. 
%              fun_data is private to imfil.m and is used in the internal
%              scaling for the function evaluation.
%
%  bounds  = N x 2 array of bound constraints on the variables.
%            These are the original bounds for the problem. We only
%            use these if you use the add_new_directions option. 
%
% Output: 
%        x = estimated minimizer
%  histout = iteration history, updated after each nonlinear iteration 
%          = N+five column array, the rows are
%            [fcount, fval, norm(sgrad), norm(step), iarm, xval]
%            fcount = cumulative function evals
%            fval = current function value
%            norm(sgrad) = current (projected) simplex grad norm
%                        = -1 means no gradient for this xval, this 
%                 can happen, for example, if you hit the target before you
%                 evaluate the simplex derivative
%            norm(step) = norm of last step 
%                       = 0 means no change, eg for stencil failure
%            iarm=line searches in current iteration to move to new point
%                =-1 means first iterate at a new scale or that the 
%                    inner iteration was terminated before the line search,
%                    eg for stencil failure
%             xval = transpose of the current iteration
%
% complete_history = complete evaluation history
%
% complete_history is a structure with the data on every evaluation of f.
% complete_history.good_points has the successful points for columns.
% complete_history.good_values is a $M x N$ matrix  of the values at
%              the good points. M > 1 for least squares.
%              good_values=f(good_points)
%
% complete_history.failed_points has the unsuccessful points for columns.
%
% You may want to use this to build surrogates, decide to add new points to
% the stencil, or for troubleshooting. The complete history can take up a
% lot of room. I will only return it as output if you ask for it, and will
% not store it at all if you set the complete_history option to 'off'.
% If I don't store it at all, then your functions to add directions to the
% stencil can't use that data.
%
% ifailed = total failure flag. I print a error message if this happens.
%
% C. T. Kelley, July 16, 2010
% This code comes with no guarantee or warranty of any kind.
%
global imfil_fscale
%
fcount=0; 
ifailed=0;
options=core_data.options;
%
% set up the difference scales and options
%
n=length(x0);
%
% In imfil_core the bounds are 0 and 1 on all the variables.
% We use the real bounds only if you use the add_new_directions
% option.
%
obounds=zeros(n,2); obounds(:,2)=1;
%
% Get the options we need.
%
imfil_explore= imfil_optread('explore',options);
imfil_executive = imfil_optread('executive',options);
imfil_function_delta = imfil_optread('function_delta',options);
imfil_least_squares  = imfil_optread('least_squares',options);
imfil_maxit          = imfil_optread('maxit',options);
imfil_maxitarm       = imfil_optread('maxitarm',options);
imfil_maxfail        = imfil_optread('maxfail',options);
imfil_noise_aware    = imfil_optread('noise_aware',options);
imfil_scale_aware    = imfil_optread('scale_aware',options);
imfil_target         = imfil_optread('target',options);
imfil_termtol        = imfil_optread('termtol',options);
stencil_wins   = imfil_optread('stencil_wins',options);
verbose        = imfil_optread('verbose',options);
imfil_complete_history = imfil_optread('complete_history',options);
complete_history=struct('good_points',[],'good_values',[],...
                            'failed_points',[]);
%
% The explore option may require some setup.
%
[explore_function, explore_data_flag, explore_data] = setup_explore(options);
%
%
% Initialize the iteration; create the stencil; set up the scales; ...
%
x=x0; xold=x0; n=length(x0); histout = [];
dscal=imfil_create_scales(options); nscal=length(dscal);
imfil_exec = options.executive;
if imfil_exec == 1
    hess = options.executive_data;
else
    hess=eye(n); 
end
xc=x0; ns=0; failc=0;
stop_now=0;
fval=imfil_target+1;
%
% Sweep through the scales.
%
sflag=1;
while (ns < nscal && fcount <= budget && failc < imfil_maxfail ...
       && stop_now == 0  && fval > imfil_target)
    ns=ns+1; h=dscal(ns); 
%
% Evaluate the function to test for instant termination and (if
% noise_aware is on) to get the estimate of the noise. Both the noise
% and the value of f  may vary as a function of the scale, so we 
% have to test it every time. 
%
% We could move this outside of the main loop if noise_aware and scale_aware
% are both off. In any case, we don't have to reevaluate f after a stencil
% failure unless f is scale-aware.
%
if imfil_noise_aware > 0 || fcount == 0 || imfil_scale_aware > 0
    [funs,iff,icf,noise_val]=feval(f,x,h,core_data);
    fval=f_to_vals(funs,imfil_least_squares);
    icount=icf;
else
    icount=0;
end
%
% The first call to f is the place to check for sanity.
%
% The first call to f also sorts out the scaling. So you can't scale the
% the targets, errors, or deltas before that call. 
%
    if fcount==0
       funerr=imfil_error_check('first_eval',funs,options,iff);
       if funerr == 1
          ifailed = 1;
          x=x0;
          histout = [];
          complete_history=[];
          break;
       end
       imfil_target=imfil_target/imfil_fscale;
       imfil_function_delta = imfil_function_delta/imfil_fscale;
       histout = [histout', [1, fval, 0, 0, 0, x']']';
%
% Initialize the internal data structures.
%
       itc=0;
       stencil_data = ...
           create_stencil_data(options, imfil_fscale, noise_val, bounds);
       iteration_data=struct('h',h,'obounds',obounds,'itc',itc, ...
               'xb',x,'fobjb',fval,'funsb',funs,...
               'complete_history',complete_history,...
               'f_internal',f,'core_data',core_data,'options',options);
    else
%
% End of first-call-to-f block
%
% If it's not the first call to f, update the internal data structures.
%
       iteration_data.h=h;
       stencil_data.noise_val = noise_val;
    end
    if imfil_complete_history > 0
       complete_history=iteration_data.complete_history;
       complete_history = ...
            single_point_hist_update(complete_history,x,funs,iff);
       iteration_data.complete_history=complete_history;
    end
    fcount=fcount+icf;
    if fval < imfil_target 
       stop_now=1;
       break;
    end
    stol=imfil_termtol*h; iarm=0; nfail=0;
%
% Compute the stencil gradient to prepare for the quasi-Newton iteration.
%
    [sdiff,sgrad,npgrad,fcount,sflag,jac,iteration_data,stop_now]...
          = manage_stencil_diff(x,f,funs,...
                  iteration_data,fcount,stencil_data,stop_now);
    histout = [histout', [fcount, fval, npgrad, -1, -1, x']']';
    gc=sgrad;
%
%
    if npgrad < stol || sflag==0 || stop_now==1 
%
%   Declare convergence at this scale on stencil failure or tolerance match.
%
       gc=sgrad; 
       if sflag ~= 0 
           failc=failc+1; 
       else
           failc=0;
       end
%
    else
%
%   Take a few quasi-Newton iterates. This is the inner iteration.
%
    failc=0;
%
% itc = inner iteration counter
%
    itc=0; 
%
% Newton while loop
%
    while itc < imfil_maxit*n && fval > imfil_target &&...
              npgrad >= stol && nfail==0 && fcount < budget ...
              && sflag > 0
    itc=itc+1;
    iteration_data.itc = itc;
    fc=fval;
%
%     Take an inner iteration. sdiff is the simplex derivative
%     (gradient or Jacobian) at x. gc is the simplex gradient at xc.
%
    [xp, fval, funs, fcount, iarm, iteration_data, nfail, hess] = ...
        imfil_inner_iteration(f, x, funs, sdiff, xc, gc, ...
             iteration_data, hess, fcount);
%
% Stop the entire iteration if you've hit the target.
%
    if fval < imfil_target 
       stop_now=1;
       x=xp;
       stepn=norm(xold-x,inf); 
       histout = [histout', [fcount, fval, npgrad, stepn, iarm, x']']';
       break;
    end
%
% Update xold, xc, and x. At this point the model Hessian and gc
% are evaluated at xc. xc and gc only get updated right here.
%
     xold=x; xc=x; gc=sgrad; x=xp;
%
%
% If stencil_wins is on, then take the best point you have. 
% You'll update x, but not xc.
%
     if  stencil_wins == 1
         [x, funs, fval] = write_best_to_x (iteration_data);
         stepn=norm(xc-x,inf); 
         histout = [histout', [fcount, fval, npgrad, stepn, iarm, x']']';
         nfail=0;
     end
%
% Stop on small objective function changes?
%
     fdelta=abs(fval-fc);
     if fdelta > 0 && fdelta < imfil_function_delta 
        stop_now=1;
        sflag=0;
        stepn=norm(xc-x,inf); 
        histout = [histout', [fcount, fval, npgrad, stepn, iarm, x']']';
        break;
     end
%
%    Compute the difference gradient for the next nonlinear iteration.
%
     if stop_now == 0 && fcount < budget
%       gc=sgrad;
       [sdiff,sgrad,npgrad,fcount,sflag,jac,iteration_data,stop_now]...
               = manage_stencil_diff(x,f,funs,...
                         iteration_data,fcount,stencil_data,stop_now);
     end
%
% If the quasi-Newton method terminated successfully or with a
% stencil failure, make sure x is now the best point.
%
     if stop_now == 1 || sflag == 0 
        [iteration_data, rflag] = reconcile_best_point(funs, xp, ...
           iteration_data);
        [x, funs, fval] = write_best_to_x (iteration_data);
        stepn=norm(xc-x,inf); 
        histout = [histout', [fcount, fval, npgrad, stepn, -1, x']']';
        break;
%
% Otherwise, update the history array and keep going.
%
     else
        stepn=norm(xc-x,inf); 
        histout=[histout', [fcount, fval, npgrad, stepn, iarm, x']']';
     end

%
% Update xold. It will not be the same as xc (the point for the most
% recent stencil derivative point) if you are exiting the quasi-Newton
% loop.
%
     xold=x;
   end % Newton while loop
 end % test for stencil failure for first derivative at new scale
%
% Apply the explore_function if there is one.
%
    if imfil_explore== 1
       [fcount, iteration_data] = ...
         manage_explore(explore_function,f,iteration_data,fcount,explore_data);
    end
%
%
% After the quasi-Newton iterate, I make sure that the 
% quasi-Newton point is the best I've seen. If it's not, I fix it.
% So, 'stencil_wins' is half-way on. One consequence of this is that
% I take the best point if the inner iteration fails.
%
    [iteration_data, rflag] = reconcile_best_point(funs, x, ...
           iteration_data);
    [x, funs, fval] = write_best_to_x (iteration_data);
if rflag == 1
   [mh,nh]=size(histout);
   if histout(mh,1) == fcount
      histout=histout(1:mh-1,:);
   end
   histout=[histout', [fcount, fval, npgrad, 0, 0, x']']';
end
if verbose == 1 
    [ns,fval,h,npgrad,itc,fcount]
end
end % end of loop over the scales
complete_history=iteration_data.complete_history;
function vstencil = imfil_create_stencil(options,n)
% IMFIL_CREATE_STENCIL
% Builds the stencil for imfil.m. As we evolve this we will be 
% adding the ability to do random rotations for all or part of
% a stencil and all sorts of other stuff.
%
% function vstencil = imfil_create_stencil(options,n)
%
% Input: 
%        options = imfil.m options structure
%              n = dimension
%
% Output:      
%              v = stencil
%
%
% C. T. Kelley, July 14, 2009
% This code comes with no guarantee or warranty of any kind.
%

stencil=imfil_optread('stencil',options);
vstencil=imfil_optread('vstencil',options);
if nargin ~= 2
   error('imfil_create_stencil requires two arguments');
end
%
% Is vstencil really there?
%
[mv,nv]=size(vstencil);
if mv+nv > 0
   stencil=-1;
end
%
% Build the stencil.
%
switch stencil
   case -1; % Custom stencil is ok.

   case 0 % central difference stencil
     v = [eye(n),-eye(n)];

   case 1 % one sided stencil = ON THE WAY OUT!
     v = eye(n);

   case 2 % positive basis stencil
     v = [eye(n),-ones(n,1)/sqrt(n)];

   otherwise
     error(' illegal stencil in imfil_create_stencil');
end
%
% If vstencil is a custom job, take it. Otherwise use one of
% the internal choices.
%
if stencil ~= -1
   vstencil=v;
end

function dscal = imfil_create_scales(options)
% IMFIL_CREATE_SCALES
% Creates the scales for imfil.m. Nothing much here right now, but 
% custom scales ... are in the near future.
%
% function dscal = imfil_create_scales(options)
%
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%
custom_scales=imfil_optread('custom_scales',options);
mcs=length(custom_scales);
if mcs > 0
   dscal=custom_scales;
else
   scalestart = imfil_optread('scalestart',options);
   scaledepth= imfil_optread('scaledepth',options);
%
% Do some error checking. Warn or complain as needed.
%
   if scalestart > scaledepth
      error(' imfil_create_scales: error in scales, scalestart > scaledepth');
   end
   dscal=-(scalestart:scaledepth); dscal=2.^dscal;
end
function stencil_data = ...
        create_stencil_data(options, imfil_fscale, noise_val, bounds)
% CREATE_STENCIL_DATA
%
%   Build a structure with the data, targets, errors, and deltas that
%   you need to compute the stencil derivative and  determine stencil failure.
%   This structure gets passed to manage_stencil_diff and contains everything
%   that depends only on the scale and the options. I'm doing this mostly
%   to keep the argument list from occupying several lines and confusing me.
%
% function stencil_data = ...
%      create_stencil_sdata(options, imfil_fscale, noise_val, bounds)
%
% Do not attempt to land here.
%
n = length(bounds(:,1));
v=imfil_create_stencil(options,n);
imfil_stencil_delta = imfil_optread('stencil_delta',options);
imfil_svarmin       = imfil_optread('svarmin',options);
imfil_stencil_delta = imfil_stencil_delta/imfil_fscale;
imfil_svarmin       = imfil_svarmin/imfil_fscale;
%
stencil_data=struct('stencil_delta',imfil_stencil_delta, ...
                        'svarmin',imfil_svarmin, ...
                        'noise_val', noise_val, ...
                        'bounds', bounds, 'v', v);

function [sdiff,sgrad,npgrad,fcount,sflag,jac,iteration_datap,stop_now]...
   = manage_stencil_diff(x,f,funs,iteration_data,fcount,...
        stencil_data,stop_now)
% MANAGE_STENCIL_DIFF
% This function calls stencil_diff to compute the stencil derivative,
% updates the evaluation counter and complete_history, runs through
% all the tests for stencil failure, and sends back everything
% imfil_core needs to do its job.
%
% This is an internal function. There is no reason to hack this code.
% Don't do it.
%
% This function is under development and changes frequently.
%
% C. T. Kelley, January 12, 2011
%
if stop_now == 0
%
options=iteration_data.options;
imfil_complete_history = imfil_optread('complete_history',options);
h=iteration_data.h;
%
% Unpack the stencil_data structure.
%
stencil_delta = stencil_data.stencil_delta;
svarmin = stencil_data.svarmin;
noise_val = stencil_data.noise_val;
obounds = iteration_data.obounds;
bounds = stencil_data.bounds;
v = stencil_data.v;
%
% Complete the direction matrix and compute the stencil derivative.
%
vv=imfil_augment_directions(x,v,h,options,bounds);
complete_history=iteration_data.complete_history;
[sgrad,fb,fbf,xb,icount,sflag,svar,diff_hist,jac]...
        = stencil_diff(x,f,h*vv,funs,iteration_data,complete_history);
fcount=fcount+icount;
pgrad=x - kk_proj(x-sgrad,obounds);
npgrad=norm(pgrad,inf);
%
% Run the optional stencil failure tests.
%
%
% If noise_aware = 1 and the scaled variation in f is < than the
% function's estimate of the noise, then I declare stencil failure!
%
       if max(noise_val,svarmin) > svar
           sflag=0;
       end
% If stencil_delta > 0, then I terminate the entire optimization
% when the scaled variation in f < stencil_delta. I report this as
% stencil failure as well.
%
       if stencil_delta > svar;
          stop_now=1;
          sflag=0;
       end
%
% Update the iteration_data structure.
%
iteration_datap=iteration_data;
if imfil_complete_history > 0
   complete_history = many_point_hist_update(complete_history,diff_hist);
   iteration_datap.complete_history=complete_history;
end
[iteration_datap, rflag] = reconcile_best_point(fbf, xb, ...
                      iteration_datap);
sdiff=jac_or_grad(sgrad, jac, options);
end % end of stop_now if statement
function [new_data, rflag] ...
        = reconcile_best_point(funs, x, old_data);
% RECONCILE_BEST_POINT
% function [new_data, rflag] ...
%        = reconcile_best_point(funs, x, old_data)
% 
% After the poll, or when it's time to terminate the inner or outer
% iteration, you may have a new best point.
% This function updates the record of the best point using the 
% iteration_data structure.
%
% Input: x        = current point
%        funs     = f(x)
%    old_data     = iteration_data structure    
%  least_squares  = Are we solving a nonlinear least squares problem?
%
% Output: new_data     = updated iteration_data structure    
%         fvalout = f(xout)
%         rflag = 0 if xout = x;  (ie f(x) is best, new best point)
%         rflag = 1 if xout = xb; (best point unchanged)
%
new_data=old_data;
least_squares=old_data.options.least_squares;
rflag = 1;
fb = old_data.fobjb;
fval=f_to_vals(funs,least_squares);
if fval < fb
   rflag = 0;
   new_data.xb=x;
   new_data.funsb = funs;
   new_data.fobjb = fval;
end

function [x, funs, fval] = write_best_to_x (iteration_data)
% WRITE_BEST_TO_X
% Makes the best point the current iterate.
%
% function [x, funs, fval] = write_best_to_x (iteration_data)
%
x = iteration_data.xb;
funs = iteration_data.funsb;
fval = iteration_data.fobjb;

function [sflag,best_value,best_value_f,best_point,svar,diff_hist]=...
          collect_stencil_data(good_points,good_values,failed_points,...
                  best_point_old,best_value_old,best_value_f_old,fc,options)
least_squares=imfil_optread('least_squares',options);
%
% Who's number one?
%
good_scalars=f_to_vals(good_values,least_squares);
[xbest_value,ibest]=min(good_scalars);
xbest_point=good_points(:,ibest);
xbest_value_f=good_values(:,ibest);
if xbest_value < best_value_old
   best_value = xbest_value;
   best_value_f = xbest_value_f;
   best_point = xbest_point;
else
   best_value = best_value_old;
   best_value_f = best_value_f_old;
   best_point = best_point_old;
end
%
% Find the big loser.
%
worst_value=max(good_scalars);
%
% Assemble the history structure.
%
diff_hist=struct('good_points',good_points,...
'good_values',good_values,'failed_points',failed_points);
%
% What's the total variation?
%
svar=worst_value-best_value;
%
% Stencil failure?
%
sflag=1;
if abs(best_value_old-best_value) < 1.d-14
   sflag=0;
   jac=[];
   grad=[];
end

function [xp, fval, funs, fcount, iarm, iteration_datap, nfail, hess] = ...
   imfil_inner_iteration(f, x, fx, sdiff, xc, gc, ...
             iteration_data, hess, fcount)
% IMFIL_INNER_ITERATION
% Manage the various inner iteration options.
%
% Inputs:
%         f = objective function; f returns an residual vector.
%         x = current point.
%       fun = current (vector) residual at x.
%       jac = stencil Jacobian at x.
%        xc = previous point; xc is not used in this function, but
%             that may change if we elect to put a quasi-Newton
%             method in here. xc=x for the first iteration in the 
%             inner iteration.
%        gc = stencil gradient at xc.
% iteration_data = internal structure with many goodies inside; rtfm
%   hessold = Gauss-Newton model Hessian at xc. Dummy argument
%             waiting for quasi-Newton or Levenberg-Marquardt.
%
% Output:
%      hess = Gauss-Newton model Hessian at x; no update done in here.
%             It's in the argument lists only to enable a quasi-Newton
%             update or a Levenberg-Marquardt iteration.
%        xp = new point.
%     fvalp = least squares error at xp = funs'*funs/2;
%      funs = vector residual at xp.
%      qfct = cost in function evaluations.
%      iarm = number of step size/trust region radius reductions.
% diff_hist = history data for the Gauss-Newton loop
%     nfail = 0 if the line search/trust region/LM succeeds, 1 if it fails
%
%
h = iteration_data.h;
options = iteration_data.options;
core_data = iteration_data.core_data;
imfil_least_squares=options.least_squares;
imfil_complete_history=options.complete_history;
obounds=iteration_data.obounds;
imfil_exec = options.executive;
if imfil_exec == 1
    imfil_exec_function = options.executive_function;
end
%
if imfil_exec == 0
    if imfil_least_squares == 0
         [xp, fval, funs, fct, iarm, diff_hist, nfail, hess] = ...
             imfil_qn_update(f, x, fx, sdiff, xc, gc, ...
             iteration_data, hess);
    else
         [xp, fval, funs, fct, iarm, diff_hist, nfail, hess] ...
               = imfil_gauss_newton(f, x, fx, sdiff, xc, gc, ...
                 iteration_data, hess);
    end
else
         [xp, fval, funs, fct, iarm, diff_hist, nfail, hess] ...
               = feval(imfil_exec_function,f, x, fx, sdiff, xc, gc, ...
                 iteration_data, hess);
end
fcount=fcount+fct;
iteration_datap=iteration_data;
if imfil_complete_history > 0
   iteration_datap.complete_history...
        = many_point_hist_update(iteration_data.complete_history,diff_hist);
end
[iteration_datap, rflag] = reconcile_best_point(funs, xp, iteration_datap);
function [fct,x,fval,iarm,fres,diff_hist, nfail]=...
  armijo_explore(f, sdir, fold, xc, h, core_data, obounds)
% ARMIJO_EXPLORE
% Line search for imfil.m.
%
% C. T. Kelley, September 15, 2008
%
% This code comes with no guarantee or warranty of any kind.
%
% function [fct,x,fval,iarm,fres,diff_hist, nfail]=...
%       armijo_explore(f, sdir, fold, xc, h, core_data, obounds)
%
% This is an internal function, which you are NOT TO HACK! Since
% you may hack it anyway, I will tell you want is going on. If you
% break something, may Alberich's curse be upon you!
%
% Inputs: f = objective function.
%         sdir = quasi-Newton search direction.
%
%         fold = current function value.
%
%         maxitarm = limit on number of step size reductions.
%
%         beta = stepsize reduction factor.
%
%         h = current scale.
%
% core_data = structure with the options + fun_data
%              The options are documented in optset.m.
%              fun_data is private to imfil.m and is used in the internal
%              scaling for the function evaluation.
%
%         obounds = Nx2 array of scaled bound constraints from imfil_core.
%
% Output: fct = cost of line search in function evaluations.
%
%         x = new point.
% 
%         fval = f(x).
%  
%         iarm = number of step length reductions.  
%
%         fres = residual for nonlinear least squares problems.
%
%    diff_hist = history data for this iteration.
%
%        nfail = 0 if the line search succeeds, 1 if it fails.
%
% Read the options array to get the ones we care about in the line search.
%
options = core_data.options;
imfil_parallel=imfil_optread('parallel',options);
imfil_least_squares=imfil_optread('least_squares',options);
imfil_verbose=imfil_optread('verbose',options);
beta = imfil_optread('armijo_reduction',options);
maxitarm = imfil_optread('maxitarm',options);
%
if imfil_parallel == 0
[fct,x,fval,iarm,aflag,fres,diff_hist, nfail]=...
  serial_armijo(f, sdir, fold, xc, h, obounds, core_data);
else
[fct,x,fval,iarm,aflag,fres,diff_hist, nfail]=...
  parallel_armijo(f, sdir, fold, xc, h, obounds, core_data);
end
%
if iarm == maxitarm & aflag == 1 & imfil_verbose == 1
       disp(' line search failure'); [iarm, h]
end
function [fct,x,fval,iarm,aflag,fres,diff_hist,nfail]=...
  parallel_armijo(f, sdir, fold, xc, h, obounds, core_data)
% PARALLEL_ARMIJO
% Parallel line search for imfil.m.
% Uses extra processors to explore larger stencils.
%
% function [fct,x,fval,iarm,aflag,fres]=...
%    parallel_armijo(f, sdir, fold, xc, h, obounds, core_data)
%
% Before you modify this (to use a trust region method, say), consider
% writing your own with this as a template. I'll make this very easy
% in a later version of the code. 
%
% Inputs:
%          f = objective function or residual vector, depending
%              on whether this is a least squares problem or not.
%       sdir = search direction.
%       fold = current value of the objective.
%       xc   = current point.
%   maxitarm = limit on number of step size reductions.
%       beta = reduction factor of step size. Currently beta = 1/2.
%          h = current scale.
%     obounds = Nx2 array of scaled bound constraints from imfil_core.
%
%  core_data = structure with the options + fun_data
%              The options are documented in optset.m.
%              fun_data is private to imfil.m and is used in the internal
%              scaling for the function evaluation.
% imfil_least_squares = least squares flag.
%
% Output:
%        fct = cost in function evaluations.
%          x = new point.
%       fval = objective function value at new point = fres'*fres/2
%              in the nonlinear least squares case.
%      iarm  = number of step size reductions.
%      aflag = 0 if the search finds a new point, = 1 if not.
%      fres  = residual for nonlinear least squares problems
%  diff_hist = history structure for this iteration
%      nfail = 0 if the line search succeeds, 1 if it fails.
%
%
% C. T. Kelley, July 21, 2009
% This code comes with no guarantee or warranty of any kind.
%
options = core_data.options;
imfil_least_squares=imfil_optread('least_squares',options);
imfil_limit_quasi_newton=imfil_optread('limit_quasi_newton',options);
imfil_verbose=imfil_optread('verbose',options);
beta = imfil_optread('armijo_reduction',options);
maxitarm = imfil_optread('maxitarm',options);
%
% Initialize the line search.
%
lambda=1;
n=length(xc);
fct=0;
aflag=1;
dd=sdir;
fres=[];
frest=fres;
diff_hist=struct('good_points',[],'good_values',[],'failed_points',[]);
%
if imfil_limit_quasi_newton == 1
%
% If the quasi-Newton step is much longer than the scale, shrink it.
%
% I am not convinced that this is the right thing to do, but it
% really helps most of the time.
%
 smax=10*min(h,1); if norm(dd) > smax dd=smax*dd/norm(dd); end
%
end
x=xc;
fval=fold;
%
% Evaluate all steplength choices at once.
%
number_steps=maxitarm+1;
ddm=zeros(n,number_steps);
for i=1:number_steps
   ddm(:,i)=xc-lambda*dd;
   lambda=beta*lambda;
   ddm(:,i)=kk_proj(ddm(:,i),obounds);
end
[fta,iflaga,ictra]=feval(f,ddm,h,core_data);
%
% Put the failed points in the history structure.
%
ibad=(iflaga(1:number_steps)==1);
if sum(ibad) > 0
   diff_hist.failed_points=ddm(:,ibad);
end
%
% Get the good points for the history structure.
%
igood=(iflaga(1:number_steps)==0);
if sum(igood) > 0
    diff_hist.good_points=ddm(:,igood);
    if imfil_least_squares == 1
       diff_hist.good_values=fta(:,igood);
    else
       diff_hist.good_values=fta(:,igood);
%       diff_hist.good_values=fta(igood);
    end
end
%
if imfil_least_squares == 1
     frest=fta;
end
fta=f_to_vals(fta,imfil_least_squares);
fct=fct+sum(ictra);
ilose=(iflaga==1);
fta(ilose) = fold+abs(fold)*.0001+1.d-12;
%
% Traditional duplicates a serial Armijo search. This will take
% the longest possible step and is sometimes the right thing to do.
%
% Non-traditional will take the best point from all the
% ones sampled in the search direction if the full step fails.
%
% I am still playing with this, and will let you chose in the options
% command pretty soon. 
%
traditional = 0;
if traditional == 0
%
[ft,it]=min(fta); xt=ddm(:,it);
iarm=maxitarm;
if ft < fval 
    aflag=0; fval=ft;
    if imfil_least_squares == 1
       fres=frest(:,it); 
    end
    x=xt; iarm=min(it,maxitarm-1); 
end
%
else
%
iarm=-1;
%
while iarm < maxitarm & aflag==1 
    ft=fta(iarm+2);
    xt=ddm(:,iarm+2);
    if ft < fval & aflag==1; aflag=0; 
            if imfil_least_squares == 1
                  fres=frest(:,iarm+2); 
            end
       fval=ft; x=xt; end
    iarm=iarm+1;
end
end
if iarm == maxitarm && aflag == 1 && imfil_verbose == 1
       disp(' line search failure'); [iarm, h]
end
nfail=aflag;
function [fct,x,fval,iarm,aflag,fres,diff_hist,nfail]=...
  serial_armijo(f, sdir, fold, xc, h, obounds, core_data);
% SERIAL_ARMIJO
% Serial line search for imfil.m.
%
% function [fct,x,fval,iarm,aflag,fres]=...
%   serial_armijo(f, sdir, fold, xc, h, obounds, core_data)
%
% This is a plain vanilla serial line search. Nothing to see here;
% move along.
%
% Before you modify this (to use a trust region method, say), consider
% writing your own with this as a template. I'll make this very easy
% in a later version of the code.
%
% Inputs:
%           f = objective function or residual vector, depending
%              on whether this is a least squares problem or not.
%        sdir = search direction.
%        fold = current value of the objective.
%        xc   = current point.
%           h = current scale.
%     obounds = Nx2 array of scaled bound constraints from imfil_core.
%
%   core_data = structure with the options + fun_data
%              The options are documented in optset.m.
%              fun_data is private to imfil.m and is used in the internal
%              scaling for the function evaluation.
%
% Output:
%        fct = cost in function evaluations.
%          x = new point.
%       fval = objective function value at new point = fres'*fres/2
%              in the nonlinear least squares case.
%      iarm  = number of step size reductions.
%      aflag = 0 if the search finds a new point, = 1 if not.
%      fres  = residual for nonlinear least squares problems
%  diff_hist = history structure for this iteration
%     nfail = 0 if the line search succeeds, 1 if it fails.
%
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%
options = core_data.options;
imfil_least_squares=imfil_optread('least_squares',options);
imfil_limit_quasi_newton=imfil_optread('limit_quasi_newton',options);
imfil_verbose=imfil_optread('verbose',options);
beta = imfil_optread('armijo_reduction',options);
maxitarm = imfil_optread('maxitarm',options);
%
% Initialize the line search.
%
lambda=1;
n=length(xc);
iarm=-1;
fct=0;
aflag=1;
dd=sdir;
fres=[];
frest=fres;
diff_hist=struct('good_points',[],'good_values',[],'failed_points',[]);
%
if imfil_limit_quasi_newton == 1
%
% If the quasi-Newton step is much longer than the scale, shrink it.
%
% I am not convinced that this is the right thing to do, but it
% really helps most of the time.
%
 smax=10*min(h,1); if norm(dd) > smax dd=smax*dd/norm(dd); end
%
%
end
x=xc;
fval=fold;
while iarm < maxitarm & aflag==1 
        d=-lambda*dd;
        xt=x+d;
        xt=kk_proj(xt,obounds);
        [ft,ifl,ict]=feval(f,xt,h,core_data); 
        fct=fct+ict;
        diff_hist=single_point_hist_update(diff_hist,xt,ft,ifl);
        if imfil_least_squares == 1
           frest=ft;
           ft=frest'*frest/2;
        end
        if ifl==1
           ft=fold+abs(fold)*.0001+1.d-12;
        end
        if ft < fval & aflag==1; aflag=0; fval=ft; 
                                 fres=frest; x=xt; 
        end
        if aflag==1; lambda=beta*lambda; end
    iarm=iarm+1;
end
if iarm == maxitarm & aflag == 1 & imfil_verbose == 1
       disp(' line search failure'); [iarm, h]
end
nfail = aflag;
function [xp, fvalp, funs, qfct, iarm, diff_hist, nfail, hess] ...
         = imfil_gauss_newton(f, x, fun, jac, xc,  gc, ...
                iteration_data, hessold)
% IMFIL_GAUSS_NEWTON
% Compute a damped Gauss-Newton step.
%
% function [xp, fvalp, funs, qfct, iarm, hess] ...
%         = imfil_gauss_newton(f, x, fun, jac, xc, gc, ...
%                iteration_data, hessold)
%
% You may elect to modify this routine if you don't want to spend the
% effort computing the Gauss-Newton model Hessian. The current version
% of imfil.m does not use it.
%
% Inputs: 
%         f = objective function; f returns an residual vector.
%         x = current point.
%       fun = current (vector) residual at x.
%       jac = stencil Jacobian at x.
%        xc = previous point; xc is not used in this function, but
%             that may change if we elect to put a quasi-Newton 
%             method in here.
%        gc = stencil gradient at xc.
% iteration_data = internal structure with many goodies inside; rtfm
%   hessold = Gauss-Newton model Hessian at xc. Dummy argument
%             waiting for quasi-Newton or Levenberg-Marquardt.
%
% Output:
%      hess = Gauss-Newton model Hessian at x; no update done in here.
%             It's in the argument lists only to enable a quasi-Newton
%             update or a Levenberg-Marquardt iteration.
%        xp = new point.
%     fvalp = least squares error at xp = funs'*funs/2;
%      funs = vector residual at xp.
%      qfct = cost in function evaluations.
%      iarm = number of step size reductions.
% diff_hist = history data for the Gauss-Newton loop
%     nfail = 0 if the line search succeeds, 1 if it fails
%        
%
% C. T. Kelley, January 10, 2011
% This code comes with no guarantee or warranty of any kind.
%
% Harvest what you need from iteration_data.
%
obounds=iteration_data.obounds;
options=iteration_data.options;
core_data=iteration_data.core_data;
h=iteration_data.h;
%
% Compute stencil gradient (sgrad) and least squares error (fval)
% at current point.
%
funs=fun;
fval=fun'*fun/2;
sgrad=jac'*fun;
%
hess=jac'*jac; % Return the normal equations model Hessian for now
%
imfil_verbose=imfil_optread('verbose',options);
n=length(x);
%
% Get the epsilon-active indices and encode them in a diagonal matrix.
%
epsb=1.d-6;
alist = max((x > obounds(:,1)+epsb) , (x < obounds(:,2)-epsb));
pr=diag(alist);
%
% Compute the search direction with a QR factorization.
%
rjac=jac*pr;
[rq,rr]=qr(rjac);
sdir1=(eye(n)-pr)*sgrad;
sdir2=rq'*fun;
sdir2=rr\sdir2;
sdir=sdir1+sdir2;
%
% Bound constrained line search
%
[qfct,xp,fvalp,iarm,fres,diff_hist, nfail]=armijo_explore(f, sdir, fval, x, ...
    h, core_data, obounds);
%
% If the line search fails nothing changes.
%
if length(fres) > 0
   funs=fres;
end
function [xp, fvalp, funs, qfct, iarm, diff_hist, nfail, hess] ...
           = imfil_qn_update(f, x, fval, sgrad, xc, gc, iteration_data,...
             hessold)
% IMFIL_QN_UPDATE
% function [hess, xp, fvalp, funs, qfct, iarm, diff_hist] ...
%           = imfil_qn_update(f, x, fval, sgrad, xc, gc, iteration_data,...
%             hessold)
% 
% Quasi-Newton update of point and Hessian. This function is never called
% for least squares problems.
%
% Input: 
%        f = objective function.
%        x = current point.
%     fval = f(x). 
%    sgrad = stencil gradient at current point x.
%       xc = previous point.
%       gc = stencil gradient at previous point xc.
% iteration_data = imfil internal structure
%  hessold = previous model Hessian
%
% Output:
%     hess = Quasi-Newton update Hessian at x.
%       xp = new point.
%    fvalp = f(xp)
%    funs  = fvalp; Makes calling sequence compatible with Gauss-Newton
%     qfct = cost in function evaluations.
%     iarm = number of step size reductions.
% diff_hist= history data for the quasi-Newton loop
%     nfail= 0 if line search succeeds, 1 if it fails
%
% 
% The steps are (1) update Hessian to obtain hess(current Hessian),
%               (2) use hess, gc, and the bounds to compute the new
%                   search direction,
%               (3) do a line search on that direction,
%
% C. T. Kelley, January 23, 2011
% This code comes with no guarantee or warranty of any kind.
%
core_data=iteration_data.core_data;
%
% obounds are the scaled 0-1 bounds. I use this field because my quasi-Newton
% codes were written for general bounds and I do not want to invade them
% any more than necessary.
%
obounds=iteration_data.obounds;
h=iteration_data.h;
%
% itc is the inner iteration counter. I am not updating the model Hessian
% for the first inner iteration. The reason for this is that I need a 
% couple gradients at each scale to make the quasi-Newton formula make sense.
%
itc=iteration_data.itc;
options=core_data.options;
imfil_verbose=imfil_optread('verbose',options);
quasi = imfil_optread('quasi',options);
%
nx=length(x);
hess=hessold;
%
% Update the model Hessian for all but the initial inner iteration.
%
if itc > 1
%
% Get the epsilon-inactive indices.
%
epsb=1.d-6;
alist = (x > obounds(:,1)+epsb) & (x < obounds(:,2)-epsb);
%
switch quasi
   case 1 % BFGS
      hess = bfupdate(x, xc, sgrad, gc, hessold, alist);
   case 2 % SR1
      hess = sr1up(x, xc, sgrad, gc, hessold, alist);
   otherwise % nothing
       hess = eye(nx,nx);
end
end
%
% Search direction
%
sdir = hess\sgrad;
%
% Bound constrained line search
%
[qfct,xp,fvalp,iarm,fres,diff_hist,nfail]=armijo_explore(f, sdir, fval, x, ...
    h, core_data, obounds);
funs=fvalp;

%
%   BFGS update of reduced Hessian, nothing fancy.
%
function hess = bfupdate(x, xc, sgrad, gc, hess,alist)
n=length(x);
pr=diag(alist);
y=sgrad-gc; s=x-xc; z=hess*s;
%
% Turn y into y#.
%
y=pr*y;
if y'*s > 0
   hess = pr*hess*pr + (y*y'/(y'*s)) - pr*(z*z'/(s'*z))*pr;
   hess=eye(n) - pr + hess;
end
if cond(hess) > 1.d6
    hess = eye(n);
end

%
% SR1 update of reduced Hessian.
%
function hess = sr1up(x, xc, sgrad, gc, hess,alist)
n=length(x);
pr=diag(alist);
y=sgrad-gc; s=x-xc; 
z=y - hess*s;
%
% Turn y into y#.
%
y=pr*y; z=pr*z;
if z'*s ~=0
        ptst = z'*(hess*z)+(z'*z)*(z'*z)/(z'*s);
        if ptst > 0 
            hess = pr*hess*pr + (z*z')/(z'*s); 
            hess = eye(n) - pr + hess;
        end
end
if cond(hess) > 1.d6
    hess = eye(n);
end
function [grad,best_value,best_value_f,best_point,icount,...
            sflag,svar,diff_hist,jac]=...
               stencil_diff(x,f,dx,fc,iteration_data,complete_history)
% STENCIL_DIFF 
% Stencil derivative for optimization and nonlinear least squares. 
% This function also tests for best point in stencil.
%
% function [grad,best_value,best_value_f,best_point,icount,...
%            sflag,svar,diff_hist,jac]=...
%              stencil_diff(x,f,dx,fc,iteration_data,options,h,complete_history)
%
%   Input:  x  = current point
%           f  = objective function (or vector residual for
%                nonlinear least squares)
%           dx = scaled difference directions
%           fc = current function value
%
%   iteration_data = structure with the options + fun_data +
%                    current iteration parameters.
%               The options are documented in optset.m.
%               fun_data is private to imfil.m and is used in the internal
%               scaling for the function evaluation.
%
% complete_history = every point imfil has ever seen. That data are used
%                    here to prevent redundant evaluations.
%
%   Output: 
%         grad = stencil gradient; NaN if every point on the stencil is bad
%  best_value  = best value in stencil
%  best_value_f= best least squares residual in stencil
%  best_point  = best point in stencil
%       icount = counter of calls to expensive part of function
%                 the function needs to return this.
%        sflag = 0 current point is best in the stencil
%                     This means stencil failure.
%
%              = 1 means there's a better point on the stecil.
%
%         svar = variation on stencil = max - min
%      
%    diff_hist = every function evaluation for points satisfying the 
%                 bounds stored in a nice struct. This is used to update
%                 the complete_history structure.
%
%          jac = stencil Jacobian for least squares problems.
%
% C. T. Kelley, July 30, 2009
% This code comes with no guarantee or warranty of any kind.
%
core_data=iteration_data.core_data;
h=iteration_data.h;
%
% bounds = iteration_data.obounds are the 0-1 bounds for the 
% scaled problem.
%
bounds=iteration_data.obounds;
%
options = core_data.options;
parallel=imfil_optread('parallel',options);
least_squares=imfil_optread('least_squares',options);
%
% Poll the stencil and collect some data.
%
[best_value,best_value_f,best_point,icount,sgood,good_points,good_values,...
         good_dx,good_df,failed_points] = ...
           imfil_poll_stencil(x,f,dx,fc,bounds,core_data,h,complete_history);
%
% Initialize the search for best value at the center.
%
if least_squares == 1
fval = fc'*fc/2; % best value in the stencil
    else
fval = fc;
end
%
% Use the points and values to get the stencil statisitcs.
%
[sflag,best_value,best_value_f,best_point,svar,diff_hist]=...
    collect_stencil_data(good_points,good_values,failed_points,...
                x,fval,fc,fc,options);
%
% sgood = 0 means there are no good points. Shrink time!
%
% Estimate the derivative.
% The idea is that fprime*dx approx df, where
% fprime   = m x n is the gradient-transpose or the Jacobian
% good_dx  = n x pnew (pnew <= vsize) is the matrix of good steps
% good_df  = m x pnew is the collection of good function values at x + dx
% 
% So the least squares problem to be solved is 
%         min || fprime * good_dx - good_df ||
% for the columns of fprime. The minimum norm solution is
% fprime = good_df*pinv(good_dx).
%
if sgood > 0
   pdx=pinv(good_dx);
   fprime=good_df*pdx;
   if least_squares == 0
      grad=fprime';
      jac=[];
   else
      jac=fprime;
      grad=fprime'*fc;
   end
else
   grad=NaN*x;
   jac=[];
end
function px = kk_proj(x,bounds)
% KK_PROJ
% Projection onto the feasible hyperrectangle.
%
% function px = kk_proj(x,bounds)
%
% Not exciting stuff.
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%
ndim=length(x);
px=zeros(ndim,1);
px=min(bounds(:,2),x);
px=max(bounds(:,1),px);
function fvals=f_to_vals(funs,least_squares)
%
% F_TO_VALS
% Evaluate fvals = f^T f/2 when f is a vector least squares residual.
%
% function fvals=f_to_vals(funs,least_squares)
%
% There is no reason you'd want to mess with this.
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%
[m,n]=size(funs);
if m == 0 && n == 0
   fvals=[];
end
fvals=zeros(n,1);
if least_squares == 1
  for i=1:n
    fvals(i)=funs(:,i)'*funs(:,i)/2;
  end
else
    fvals=funs;
end

function vout=imfil_augment_directions(x,vin,h,options,bounds)
% IMFIL_AUGMENT_DIRECTIONS
% Enrich the stencil with random directions and a user-supplied
% new_direcions function.
%
% C. T. Kelley, August 13, 2009.
% This code comes with no guarantee or warranty of any kind.
%
% Add the random directions.
%
random_stencil = imfil_optread('random_stencil',options);
vout= random_augment(vin,random_stencil);
%
% See if there's a new_directions function.
%
new_directions = imfil_optread('add_new_directions',options);
lnew=length(new_directions);
if lnew > 0
    dbv=bounds(:,2)-bounds(:,1); db=diag(dbv);
    unscaled_x = db*x + bounds(:,1);
%    unscaled_v = db*vin;
    unscaled_v = db*vout;
    unscaled_vnew=feval(new_directions, unscaled_x, h, unscaled_v);
    [mv,nv]=size(unscaled_vnew);
    if mv > 0
        vnew=inv(db)*unscaled_vnew;
        [mv,nv]=size(vnew);
        for i=1:nv
           vnew(:,i)=vnew(:,i)/norm(vnew(:,i),inf);
        end
        vout=[vout, vnew];
    end
end

function vout=random_augment(vin,k)
% RANDOM_AUGMENT
% Add k random unit vectors to the stencil.
%
% This makes some of the theory work, but it is not always
% a good idea to do this. Adding more vectors makes stencil failure
% less likely, which is not good if you have a full stencil and you're
% wasting lots of time in the line search.
%
% Adding random vectors makes more sense if the minimizer is 
% on a hidden or explicit constraint boundary.
%
% C. T. Kelley, January 5, 2011
% This code comes with no guarantee or warranty of any kind.
%
[n,nn]=size(vin);
if k==0
   vout=vin;
end
%
% Generate k uniformly distributed points on the unit sphere.
% See Marsaglia, G. "Choosing a Point from the Surface of a Sphere." 
%     Ann. Math. Stat. 43, 645-646, 1972.
%
rv=randn(n,k);
for ir=1:k
   rv(:,ir)=rv(:,ir)/norm(rv(:,ir));
end
vout=[vin,rv];

function [best_value,best_value_f,best_point,icount,sgood,...
      good_points,good_values,good_dx,good_df,failed_points] = ...
              imfil_poll_stencil(x,f,dx,fc,bounds,core_data,h,complete_history)
% IMFIL_POLL_STENCIL
% Poll the stencil and organize the results.
% I do not think you will want to modify this code.
%
% function [best_value,best_point,icount,sgood,good_points,good_values,...
%           good_dx,good_df,failed_points] = ...
%                          imfil_poll_stencil(x,f,dx,fc,bounds,options,h)
%
% Input:
%        x = center of stencil
%        f = objective function
%       dx = h*V = array of scaled directions
%       fc = f(x)
%   bounds = N x 2 array of lower/upper bounds
%  core_data = imfil core_data structure
%        h = current scale
%
% Output:
%
% The best_point data is frozen at xc for this function. We only keep
% track of it here so we can ship it to 
%     best_point    = f(best_point) == best_value
%     best_value    = lowest value of f on the stencil
%     best_value_f  = best least squares residual
%     icount        = cost in units of calls to f
%     sgood         = number of successful evaluations
%     good_points   = list of points at which f returned a value
%     good_values   = f(good_points)
%     good dx       = vector of good_points - x
%     good df       = vector of f(good_points) - fc
%     failed_points = list of points at which f did not return a value
%
% C. T. Kelley, Aug 13, 2009
% This code comes with no guarantee or warranty of any kind.
%
options=core_data.options;
parallel=imfil_optread('parallel',options);
least_squares=imfil_optread('least_squares',options);
[n,vsize]=size(dx);
%
% Record the best point and best function value in the stencil.
% Get started by using the center point.
%
best_point=x;
if least_squares == 1
best_value=fc'*fc/2; % best value in the stencil
    else
best_value=fc;
end
best_value_f=fc;
%
iflag=zeros(vsize,1);
m=length(fc); % m > 1 tells me it's a least squares problem
fp=zeros(m,vsize);
failed_points=[];
good_points=[];
good_values=[];
sgood=0;
fval=zeros(vsize,1);
icount=0;
%
% fp(:,i) and fval(i) are not defined outside of the bounds
% or if iflag(i)=1.
%
% First cull the points which violate the bound constraints.
%
% Collect the feasible points, differences, and functions in xp1 and dx1.
%
%
pold=0;
dx1=[];
xp1=[];
%
% One-sided differences may need to flip the direction if the positive
% perturbation is not feasible.
%
stencil_type=options.stencil;
if stencil_type == 1
   for i=1:vsize
     xp(:,i)=x+dx(:,i);
     if isok(xp(:,i),bounds) == 0
        dx(:,i)=-dx(:,i);
     end
   end
end
%
%
%
for i=1:vsize
  xp(:,i)=x+dx(:,i);
  if isok(xp(:,i),bounds)
     pold=pold+1;
     dx1=[dx1,dx(:,i)];
     xp1=[xp1,xp(:,i)];
  end
end
fp1=fp(:,1:pold);
xp=xp1; dx=dx1; fp=fp1;
%
% Query the complete_history structure to see if you've evaluated f
% at any of these points before.
%
[oldindex,oldpoints,oldvalues,oldflags]= ...
               scan_history(complete_history,xp,fp,dx);
newindex=~oldindex;
xp=xp1(:,newindex);
iflago=zeros(1,vsize);
if sum(oldindex) > 0
   fp(:,oldindex)=oldvalues;
   iflago(oldindex)=oldflags;
end
%xp=xpout; dx=dxout; fp=fpout;
%
% Evaluate f, in parallel if possible. Flag the failed points.
%
pnew=sum(newindex);
fp1=[];
iflag=[];
if parallel == 0
    for i=1:pnew
        [fpx,iflagx,ict]=feval(f,xp(:,i),h,core_data);
        fp1=[fp1,fpx];
        iflag=[iflag,iflagx];
        icount=icount+ict;
    end
else
    if pnew > 0
        [fp1,iflag,ictrp]=feval(f,xp,h,core_data);
        icount=icount+sum(ictrp);
    end
end
if pnew > 0
   fp(:,newindex)=fp1;
   iflago(newindex)=iflag;
end
fp1=fp;
iflag=iflago;
%
% Identify the failed points.
%
ibad=(iflag(1:pold)==1);
if sum(ibad) > 0
   failed_points=xp1(:,ibad);
end
%
% Store the good points and their function values.
%
igood=(iflag(1:pold)==0);
sgood=sum(igood);
good_dx=[];
good_df=[];
if sgood > 0
   good_points=xp1(:,igood);
   if least_squares == 1
      good_fp=fp1(:,igood);
   else
      good_fp=fp1(igood);
   end
   good_dx=dx1(:,igood);
   for ig=1:sgood
      if least_squares == 1
         good_df(:,ig)=good_fp(:,ig)-fc;
      else
         good_df(ig)=good_fp(ig)-fc;
      end
   end
%   good_values=f_to_vals(good_fp,least_squares);
   good_values=good_fp;
end

%
% test x for feasibility re bounds
%
function lisok=isok(x,bounds)
il=min(x >= bounds(:,1));
iu=min(x <= bounds(:,2));
isok=min(il,iu);
lisok = (isok == 1);

function sdiff = jac_or_grad(sgrad, jac, options)
% JAC_OR_GRAD
% returns either the simplex gradient or Jacobian depending
% on the type of problem (optization or least squares)
%
% function sdiff = jac_or_grad(sgrad, jac, options)
%
imfil_least_squares=options.least_squares;
switch imfil_least_squares
   case 1
     sdiff=jac;
   case 0 
     sdiff=sgrad;
   otherwise
     disp(' error in jac_or_grad ')
end
function [fctout, iteration_datap] = ...
         manage_explore(explore_function,f,iteration_data,fctin,explore_data);
% MANAGE_EXPLORE
% Call the user-defined explore function and update the counter
% and the iteration_data structure.
%
iteration_datap=iteration_data;
options=iteration_data.options;
imfil_complete_history=imfil_optread('complete_history',options);
%
[xs, fs, my_cost, explore_hist] = ...
        feval(explore_function,f,iteration_data,explore_data);
fctout=fctin + my_cost;
%
if imfil_complete_history > 0
   iteration_datap.complete_history...
      = many_point_hist_update(iteration_datap.complete_history,explore_hist);
end
%
[iteration_datap, rflag] = reconcile_best_point(fs, xs, iteration_datap);
function new_hist=many_point_hist_update(old_hist,diff_hist)
% MANY_POINT_HIST_UPDATE
% function new_hist=many_point_hist_update(old_hist,diff_hist)
%
% Update the complete_history structure after a many calls to f.
%
%
% WARNING! The complete_history structure uses your bounds, and is
% not scaled to make 0 <= x(i) <= 1.
%
new_hist=old_hist;
new_hist.good_points=[new_hist.good_points,diff_hist.good_points];
new_hist.good_values=[new_hist.good_values,diff_hist.good_values];
new_hist.failed_points=[new_hist.failed_points,diff_hist.failed_points];
function new_hist=single_point_hist_update(old_hist,x,fout,ifail)
%SINGLE_POINT_HIST_UPDATE
% Update the complete_history structure after a single call to f.
%
% function new_hist=single_point_hist_update(old_hist,x,fout,ifail)
%
%
new_hist=old_hist;
%
% Write the data.
%
if ifail == 1
    new_hist.failed_points=[old_hist.failed_points,x];
else
    new_hist.good_points=[old_hist.good_points,x];
    new_hist.good_values=[old_hist.good_values,fout];
end
function [oldindex,oldpoints,oldvalues,oldflags] = ...
               scan_history(complete_history,xp,fp,dx)
[mp,np]=size(xp);
imold=zeros(1,np);
oldpoints=[];
oldvalues=[];
oldflags=[];
for i=1:np
    [fpt,ift]=scal_complete_history(complete_history,xp(:,i));
    if ift > -1
       oldpoints=[oldpoints,xp(:,i)];
       oldvalues=[oldvalues,fpt];
       oldflags=[oldflags,ift];
       imold(i)=1;
    end
end
oldindex=(imold==1);


function [fhist,iflaghist]=scal_complete_history(complete_history,x)
%fhist=[];
iflaghist=-1;
losers=complete_history.failed_points;
[ml,nl]=size(losers);
winners=complete_history.good_points;
[mw,nw]=size(winners);
fhist=NaN(mw,1);
iquit=0;
if nw > 0
for i=1:nw
  d=norm(x-winners(:,i),inf);
  if d < 1.d-12
     iquit=1;
     fhist=complete_history.good_values(:,i);
     iflaghist=0;
     break;
  end
end
end
if iquit == 0 & nl > 0
for i=1:nl
   d=norm(x-losers(:,i),inf);
   if d < 1.d-12
     fhist=NaN;
     iquit=1;
     iflaghist=1;
     break;
  end
end
end
function [explore_function, explore_data_flag, explore_data] = ...
        setup_explore(options)
% SETUP_EXPLORE
% Gets the explore function organzied if you have one.
%
imfil_explore= imfil_optread('explore',options);
explore_function=[];
explore_data_flag=[];
explore_data=[];
if imfil_explore== 1
   explore_function=imfil_optread('explore_function',options);
   explore_data_flag=imfil_optread('explore_data_flag',options);
   if explore_data_flag == 1
      explore_data = imfil_optread('explore_data',options);
   end
end
function valout=imfil_optread(str,optin)
% IMFIL_OPTREAD
%
% Reads the options one at a time. This is an internal
% function for imfil.m. 
%
% Input: 
%      str = name of option
%    optin = options structure
%
% Output: 
%   valout = value of option
%
% Example: fscale=imfil_optread('fscale',optin);
%
% The options array is fully documented in the comment lines to
% imfil_optset. Type `help imfil_optset' for the details.
%
% There is no error checking in imfil_optread. 
% 
%
% C. T. Kelley, September 15, 2008
% This code comes with no guarantee or warranty of any kind.
%

str=lower(str);
if nargin <= 2
   n=-1;
end
parms=fieldnames(optin);
nopt=length(parms);
did_ok=0;
for p=1:nopt
    if strcmp(str,parms(p));
       valout=getfield(optin,str);
       did_ok=1;
    end
end

if did_ok == 0;
   error('Error in imfil_optread');
end

function eflag=imfil_error_check(varargin)
% IMFIL_ERROR_CHECK
%
% Checks dimensions and options for input errors or inconsistencies.
% This function is not intended for use outside of imfil.m. It does
% not error check itself.
%
% C. T. Kelley, Feb 19, 2010
%
str=varargin{1};
switch str
   case 'bounds'
       x=varargin{2};
       bounds=varargin{3};
       eflag=imfil_check_bounds(x,bounds);
   case 'first_eval'
       funs=varargin{2};
       options=varargin{3};
       iflag=varargin{4};
       eflag=imfil_check_first_eval(funs,options,iflag);
   otherwise
       emsg=strcat('"',str,'"',' is an illegal argument to error_check.');
       disp(emsg);
end
%
function lsqerr = imfil_check_first_eval(funs,options,iflag)
% IMFIL_CHECK_FIRST_EVAL
%
% Complain if you have a scalar-valued function + nonlinear least squares.
% Give up if you have a vector-valued function + not a least squares problem.
% Warn if the function fails to return a value at the initial iterate.
%
lsqerr=0;
[mf,nf]=size(funs);
lsq=options.least_squares;
if mf > 1 && lsq == 0
mf
 disp('Your function is vector-valued but the least_squares option is off.'); 
 disp('imfil cannot run with this inconsistency.');
 lsqerr=1;
end
if mf == 1 && lsq == 1
 disp('Your function is scalar-valued but the least_squares option is on.'); 
 disp('Are you sure this is what you want to do?');
end
if iflag > 0
 disp('Your function failed to return a value at the initial iterate.');
 disp('This is usually a problem. You have been warned.');
end
%
function badargs = imfil_check_bounds(x,bounds);
% IMFIL_CHECK_BOUNDS
%
% Sanity check for the bounds. Complain if the bounds are not the 
% same length as x, if the bounds are not properly arranged into 
% columns, if the lower bound is not less than the upper bound,
% or if x is infeasible.
%
badargs=0;
[mx,nx]=size(x);
[mb,nb]=size(bounds);
if mb ~= mx
   badargs=1;
   disp(' The columns [lower, upper] of the bounds array must have');
   disp(' same length as x. ');
end
if nb ~= 2
   badargs=1;
   disp(' The bounds array must have two columns [lower, upper].');
end
diff_vec=bounds(:,2) - bounds(:,1);
m_vec=min(diff_vec);
if m_vec <= 0
   badargs=1;
   disp(' The lower bound must be strictly greater than the upper bound.');
end
if m_vec > 0
   px=max(bounds(:,1),min(x,bounds(:,2))); 
   nd=norm(x-px);
   if nd > 0
      badargs=1;
      disp(' The initial iterate is infeasible. x must satisfy the bound constraints.');
   end
end
