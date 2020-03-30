function SVM = gqr_fitsvm(x,y,ep,bc,low_rank)
% function SVM = gqr_fitsvm(x,y,ep,bc,low_rank)
% This function fits a support vector machine to given data
%   Inputs: x - data locations
%           y - classifications
%           ep - Gaussian shape parameter
%           bc - box constraint
%           low_rank - (optional, default=0) use the eigenfunction decomp
%   Output: SVM - object to classify future data
%           SVM.eval(x_new) - classify the point x_new
%           SVM.margin - the separation margin from the problem solution
%           SVM.sv_index - the indices of the support vectors

switch nargin
    case 4
        low_rank = 0;
    case 5
    otherwise
        error('Unacceptable arguments: nargin=%d',nargin)
end

N = length(y);

% Define the RBF and GQR alpha parameter as needed
rbf = @(e,r) exp(-(e*r).^2);
gqr_alpha = 1e6;

% Because the same data may get used with different ep or bc values,
% Checks below to see if the old DistanceMatrix, K and V can be reused
persistent ep_old x_old DM K V

% Record the times for setup and solve
tic
recalc_K = 0;
if isempty(ep_old) || ep_old~=ep
    ep_old = ep;
    recalc_K = 1;
end
if isempty(x_old) || any(size(x_old)~=size(x)) || any(any(x_old~=x))
    DM = DistanceMatrix(x,x);
    x_old = x;
    recalc_K = 1;
end
if recalc_K
    K = rbf(ep,DM);
    K = .5*(K + K'); % To make sure it is symmetric at machine precision
    if low_rank % Reset the GQR object as needed
        GQR = gqr_solveprep(1,x,ep,gqr_alpha);
        lamvec = sqrt(GQR.eig(GQR.Marr));
        Phi1 = gqr_phi(GQR,x);
        V = Phi1.*(y*lamvec);
    end
end
SVM.prep_time = toc;

tic
if low_rank
    % Try to compute with the low-rank approximation to the kernel matrix
    % This requires an extra set of unknowns
    %     sol_QP_gamma = Phi_1'*Lambda_1'*sol_QP_alpha
    % We don't care what gamma is though, we just want the coefficients
    %
    % NOTE: Theoretically, the sol_QP(1:N_eig) should be unbounded:
    %     lb_QP = [-Inf*ones(N_eig,1);zeros(N,1)];
    %     ub_QP = [Inf*ones(N_eig,1);bc*ones(N,1)];
    % This seems to work for most sets but I've seen complaints
    %   from quadprog:
    %     'optim:ipqpcommon:ipConvexQP:InfNaNComplexDetected'
    % I seem to be unable to avoid this problem, regardless of the bounds I
    %   choose for this problem, and I don't have any other ideas for what
    %   inf values it is finding
    N_eig = size(V,2);
    H_QP = sparse(1:N_eig,1:N_eig,ones(1,N_eig),N+N_eig,N+N_eig);
    f_QP = -[zeros(N_eig,1);ones(N,1)];
    A_QP = [];
    b_QP = [];
    Aeq_QP = [zeros(1,N_eig),y';-eye(N_eig),V'];
    beq_QP = [0;zeros(N_eig,1)];
    lb_QP = [-inf*ones(N_eig,1);zeros(N,1)];
    ub_QP = [inf*ones(N_eig,1);bc*ones(N,1)];
    x0_QP = [V'*(bc/2*ones(N,1));bc/2*ones(N,1)];
else
    % Use the standard quadratic programming formulation
    H_QP = (y*y').*K;
    f_QP = -ones(N,1); % quadprog solves the min, not the max problem
    A_QP = [];
    b_QP = [];
    Aeq_QP = y';
    beq_QP = 0;
    lb_QP = zeros(N,1);
    ub_QP = bc*ones(N,1);
    x0_QP = bc/2*ones(N,1);
end
optimopt_QP = optimset('LargeScale','off','Display','off','MaxIter',500);
SVM.setup_time = toc;

% Solve the quadratic program
tic
[sol_QP,fval,SVM.exitflag,SVM.output] = quadprog(H_QP,f_QP,A_QP,b_QP,Aeq_QP,beq_QP,lb_QP,ub_QP,x0_QP,optimopt_QP);
if SVM.exitflag~=1
    error('Failed quadratic program solve')
end
SVM.solve_time = toc;

tic
if low_rank
    sol_QP = sol_QP(N_eig+1:N_eig+N); % Only take the right values
end

% This is one way to measure how well the SVM is doing
% The quadratic program minimizes
%    .5*x'*H*x + f'x,   x is sol_QP
% So we can undo that to find x'*H*x which is the margin
% The sum appears because f_QP is all ones
SVM.margin = sqrt(.5/(fval + sum(sol_QP)));

% Create the coefficients and identify the support vectors
% A fudge factor is created to allow for slightly nonzero values
svm_fuzzy_logic = min(1e-5,1e-3*bc);
coef = y.*sol_QP;
SVM.sv_index = sol_QP>svm_fuzzy_logic;

% To solve for b, we can just compute
%    b = y_i - sum_j=1^n alpha_i*y_i K(x_i,x_j)
% but only for i such that 0<alpha_i<C, not <=
% I take the mean of all such values, but they should all be the same
bias_find_coef = sol_QP>svm_fuzzy_logic & sol_QP<bc-svm_fuzzy_logic;
% NOTE: It's possible no such point will exist, maybe
%       If that is the case, set the bias to 0, I guess
if sum(bias_find_coef)
    bias = mean(y(bias_find_coef) - K(bias_find_coef,:)*coef);
else
    bias = 0;
end

% These values are returned to the user but should not be used to evaluate
% the SVM.  Only SVM.eval should be used for that.
SVM.coef = coef;
SVM.bias = bias;

% Create a function to evaluate the SVM
SVM.eval = @(x_new) sign(rbf(ep,DistanceMatrix(x_new,x(SVM.sv_index,:)))*coef(SVM.sv_index) + bias);
SVM.postsolve_time = toc;