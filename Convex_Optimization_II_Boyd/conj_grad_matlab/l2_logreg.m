function [w,status,history] = l2_logreg(X,b,lambda,method,ptol,pmaxi)
%
% l2-regularized Logistic Regression Problem Solver
%
% L2_LOGREG solves problems of the following form:
%
% minimize (1/m) sum_i log(1+exp(-b_i(x_i*w))) + sum_j lambda_i*w_j^2,
% 
% where variable is w and problem data are x_i, b_i and lambda_i.
%
% INPUT
%
%  X       : mxn matrix; input data. each row corresponds to each feature
%  b       : m vector; class label
%  lambda  : positive  n-vector; regularization parameter
%
%  method  : string; search direction method type
%               'cg'   : conjugate gradients method, 'pcg'
%               'pcg'  : preconditioned conjugate gradients method
%               'exact': exact method (default value)
%  ptol    : scalar; pcg relative tolerance. if empty, use adaptive rule.
%  pmaxi   : scalar: pcg maximum iteration. if empty, use default value (500).
%
% OUTPUT
%
%  w       : n vector; classifier
%  status  : scalar; +1: success, -1: maxiter exceeded
%  history :
%            row 1) phi
%            row 2) norm(gradient of phi)
%            row 3) cumulative cg iterations
%
% USAGE EXAMPLE
%
%  [w,status] = l2_logreg(X,b,lambda,'pcg');
%

% Written by Kwangmoo Koh <deneb1@stanford.edu>

%------------------------------------------------------------
%       INITIALIZE
%------------------------------------------------------------

% NEWTON PARAMETERS
MAX_TNT_ITER    = 200;      % maximum (truncated) Newton iteration
ABSTOL          = 1e-8;     % terminates when the norm of gradient < ABSTOL

% LINE SEARCH PARAMETERS
ALPHA           = 0.01;     % minimum fraction of decrease in norm(gradient)
BETA            = 0.5;      % stepsize decrease factor
MAX_LS_ITER     = 100;      % maximum backtracking line search iteration

[m,n]   = size(X);          % problem size: m examples, n features

if(isempty(pmaxi)) pcgmaxi = 500; else pcgmaxi = pmaxi; end
if(isempty(ptol )) pcgtol = 1e-4; else pcgtol  = ptol;  end

% INITIALIZE
pobj  = Inf; s = inf; pitr  = 0 ; pflg  = 0 ; prelres = 0; pcgiter = 0;
history = [];

w = zeros(n,1); dw =  zeros(n,1);

A = sparse(1:m,1:m,b)*X;
%if (strcmp(method,'cg')||strcmp(method,'pcg')) A2 = A.^2; end
A2 = A.^2;


disp(sprintf('%s %15s %10s %10s %6s %10s %6s',...
    'iter','primal obj','stepsize','norg(g)','p_flg','p_res','p_itr'));

%------------------------------------------------------------
%               MAIN LOOP
%------------------------------------------------------------

for ntiter = 0:MAX_TNT_ITER
    Aw = A*w; expAw = exp(Aw); expmAw = exp(-Aw);
    g = -1/m./(1+expAw); h = 1/m./(2+expAw+expmAw);
    gradphi = A'*g+2*lambda.*w;
    normg = norm(gradphi);

    phi = sum(log(1+expmAw))/m+sum(lambda.*w.*w);
    disp(sprintf('%4d %15.6e %10.2e %10.2e %6d %10.2e %6d',...
                ntiter,phi,s,normg,pflg,prelres,pitr));
    history = [history [phi; normg; pcgiter]];

    %------------------------------------------------------------
    %   STOPPING CRITERION
    %------------------------------------------------------------
    if (norm(gradphi) < ABSTOL) 
        status = 1;
        disp('Absolute tolerance reached.');
        disp(sprintf('%d/%d',sum(abs((A2'*h)./(2*lambda))<0.5),n));
        return;
    end
    %------------------------------------------------------------
    %       CALCULATE NEWTON STEP
    %------------------------------------------------------------
    switch lower(method)
        case 'pcg'
        if (isempty(ptol)) pcgtol = min(0.1,norm(gradphi)); end
        [dw, pflg, prelres, pitr, presvec] = ...
            pcg(@AXfunc,-gradphi,pcgtol,pcgmaxi,@Mfunc,[],[],...
                A,h,2*lambda,1./(A2'*h+2*lambda));
        if (pitr == 0) pitr = pcgmaxi; end

        case 'cg'
        if (isempty(ptol)) pcgtol = min(0.1,norm(gradphi)); end
        [dw, pflg, prelres, pitr, presvec] = ...
            pcg(@AXfunc,-gradphi,pcgtol,pcgmaxi,[],[],[],...
                A,h,2*lambda,[]);
        if (pitr == 0) pitr = pcgmaxi; end

        otherwise % exact method
        hessphi = A'*sparse(1:m,1:m,h)*A+2*sparse(1:n,1:n,lambda);
        dw = -hessphi\gradphi;
    end
    pcgiter = pcgiter+pitr;
    %------------------------------------------------------------
    %   BACKTRACKING LINE SEARCH
    %------------------------------------------------------------
    s = 1;
    for lsiter = 1:MAX_LS_ITER
        new_w = w+s*dw;
        newgradphi = A'*(-1/m./(1+exp(A*new_w)))+2*lambda.*new_w;
        if (norm(newgradphi)<=(1-ALPHA*s)*normg) break; end
        s = BETA*s;
    end
    if (lsiter == MAX_LS_ITER) break; end
    w = new_w;
end
status = -1;

%------------------------------------------------------------
%   CALL BACK FUNCTIONS FOR PCG
%------------------------------------------------------------
function y = AXfunc(x,A,h,d,p)
    y = A'*(h.*(A*x))+d.*x;

function y = Mfunc(x,A,h,d,p)
    y = x.*p;

