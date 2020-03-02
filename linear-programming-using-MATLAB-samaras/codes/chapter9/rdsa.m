function [xsol, fval, exitflag, iterations] = ...
    rdsa(A, c, b, Eqin, MinMaxLP, c0, reinv, tole1, ...
    tole2, tole3, scalingTechnique, basisUpdateMethod)
% Filename: rdsa.m
% Description: the function is an implementation of the 
% revised dual simplex algorithm
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [xsol, fval, exitflag, iterations] = ...
%   rdsa(A, c, b, Eqin, MinMaxLP, c0, reinv, tole1, ...
%   tole2, tole3, scalingTechnique, basisUpdateMethod)
% 
% Input:
% -- A: matrix of coefficients of the constraints 
%    (size m x n)
% -- c: vector of coefficients of the objective function 
%    (size n x 1)
% -- b: vector of the right-hand side of the constraints 
%    (size m x 1)
% -- Eqin: vector of the type of the constraints 
%    (size m x 1)
% -- MinMaxLP: the type of optimization (optional: 
%    default value -1 - minimization)
% -- c0: constant term of the objective function
%    (optional: default value 0)
% -- reinv: every reinv number of iterations, the basis
%    inverse is re-computed from scratch (optional:
%    default value 80)
% -- tole1: tolerance for the basic solution (optional: 
%    default value 1e-07)
% -- tole2: tolerance for the reduced costs (optional: 
%    default value 1e-09)
% -- tole3: tolerance for the pivoting column (optional: 
%    default value 1e-09)
% -- scalingTechnique: the scaling method to be used
%    (0: no scaling, 1: arithmetic mean, 2: de Buchet for 
%    the case p = 1, 3: de Buchet for the case p = 2, 
%    4: de Buchet for the case p = Inf, 5: entropy, 
%    6: equilibration, 7: geometric mean, 8: IBM MPSX, 
%    9: LP-norm for the case p = 1, 10: LP-norm for 
%    the case p = 2, 11: LP-norm for the case p = Inf) 
%    (optional: default value 6)
% -- basisUpdateMethod: the basis update method to be used
%    (1: PFI, 2: MPFI) (optional: default value 1)
%
% Output:
% -- xsol: the solution found (size m x 1)
% -- fval: the value of the objective function at the 
%    solution x
% -- exitflag: the reason that the algorithm terminated 
%    (0: optimal solution found, 1: the LP problem is
%    infeasible, 2: the LP problem is unbounded, -1: the
%    input data is not logically or numerically correct)
% -- iterations: the number of iterations

% initialize output variables
xsol = [];
fval = 0;
exitflag = 0;
iterations = 0;
% set default values to missing inputs
if ~exist('MinMaxLP')
	MinMaxLP = -1;
end
if ~exist('c0')
	c0 = 0;
end
if ~exist('reinv')
	reinv = 80;
end
if ~exist('tole1')
	tole1 = 1e-7;
end
if ~exist('tole2')
	tole2 = 1e-9;
end
if ~exist('tole3')
	tole3 = 1e-9;
end
if ~exist('scalingTechnique')
	scalingTechnique = 6;
end
if ~exist('basisUpdateMethod')
	basisUpdateMethod = 1;
end
[m, n] = size(A); % find the size of matrix A
[m2, n2] = size(c); % find the size of vector c
[m3, n3] = size(Eqin); % find the size of vector Eqin
[m4, n4] = size(b); % find the size of vector b
% check if input data is logically correct
if n2 ~= 1
	disp('Vector c is not a column vector.')
	exitflag = -1;
	return
end
if n ~= m2
	disp(['The number of columns in matrix A and ' ...
        'the number of rows in vector c do not match.'])
	exitflag = -1;
	return
end
if m4 ~= m
	disp(['The number of the right-hand side values ' ...
        'is not equal to the number of constraints.'])
	exitflag = -1;
	return
end
if n3 ~= 1
	disp('Vector Eqin is not a column vector')
	exitflag = -1;
	return
end
if n4 ~= 1
	disp('Vector b is not a column vector')
	exitflag = -1;
	return
end
if m4 ~= m3
	disp('The size of vectors Eqin and b does not match')
	exitflag = -1;
	return
end
% if the type of optimization is maximization, then multiply 
% vector c and constant c0 by -1
if MinMaxLP == 1
	c = -c;
	c0 = -c0;
end
% perform the presolve analysis
disp('---- P R E S O L V E    A N A L Y S I S ----')
[A, c, b, Eqin, c0, infeasible, unbounded] = ...
	presolve(A, c, b, Eqin, c0);
if infeasible == 1 % the LP problem is infeasible
    disp('The LP problem is infeasible')
	exitflag = 1;
	return
end
if unbounded == 1 % the LP problem is unbounded
    disp('The LP problem is unbounded')
	exitflag = 2;
	return
end
[m, n] = size(A); % find the new size of matrix A
[m2, ~] = size(c); % find the new size of vector c
[m3, ~] = size(Eqin); % find the size of vector Eqin
% scale the LP problem using the selected scaling
% technique
disp('---- S C A L I N G ----')
if scalingTechnique == 1 % arithmetic mean
	[A, c, b, ~, ~] = arithmeticMean(A, c, b);
% de buchet for the case p = 1
elseif scalingTechnique == 2
	[A, c, b, ~, ~] = debuchet1(A, c, b);
% de buchet for the case p = 2
elseif scalingTechnique == 3
	[A, c, b, ~, ~] = debuchet2(A, c, b);
% de buchet for the case p = Inf
elseif scalingTechnique == 4
	[A, c, b, ~, ~] = debuchetinf(A, c, b);
elseif scalingTechnique == 5 % entropy
	[A, c, b, ~, ~] = entropy(A, c, b);
elseif scalingTechnique == 6 % equilibration
	[A, c, b, ~, ~] = equilibration(A, c, b);
elseif scalingTechnique == 7 % geometric mean
	[A, c, b, ~, ~] = geometricMean(A, c, b);
elseif scalingTechnique == 8 % IBM MPSX
	[A, c, b, ~, ~, ~, ~] = ibmmpsx(A, c, b);
% LP-norm for the case p = 1
elseif scalingTechnique == 9
	[A, c, b, ~, ~] = lpnorm1(A, c, b);
% LP-norm for the case p = 2
elseif scalingTechnique == 10
	[A, c, b, ~, ~] = lpnorm2(A, c, b);
% LP-norm for the case p = Inf
elseif scalingTechnique == 11
	[A, c, b, ~, ~] = lpnorminf(A, c, b);
end
% find an invertible basis
disp('---- I N I T I A L    B A S I S ----')
flag = isequal(Eqin, zeros(m3, 1));
if flag == 1 % all constraints are equalities
	% select an initial invertible basis using lprref 
	% function
	[~, ~, jb, out, ~, exitflag] = lprref(A, ...
        b, Eqin, 1e-10);
	if exitflag == 1 % the LP problem is infeasible
        disp('The LP problem is infeasible')
        return
	end
	% create the basic and nonbasic lists
	BasicList = jb;
	NonBasicList = setdiff(1:n, BasicList);
	% delete redundant constraints found by lprref
	A(out, :) = [];
	b(out, :) = [];
	Eqin(out, :) = [];
else % some or all constraints are inequalities
	% add slack variables
	axm = nnz(Eqin);
	c(m2 + 1:m2 + axm, :) = 0;
	A(:, n + 1:n + axm) = sparse(m, axm);
	curcol = 1;
	for i = 1:m3
        % 'greater than or equal to' inequality constraint
        if Eqin(i, 1) == 1
            A(i, n + curcol) = -1;
            curcol = curcol + 1;
        % 'less than or equal to' inequality constraint
        elseif Eqin(i, 1) == -1
            A(i, n + curcol) = 1;
            curcol = curcol + 1;
        % unrecognized type of constraint
        elseif Eqin(i,1) ~= 0
            disp('Vector Eqin is not numerically correct.')
            exitflag = -1;
            return
        end
	end
	% select an initial invertible basis using lprref 
	% function
	[~, ~, jb, out, ~, exitflag] = lprref(A, ...
        b, Eqin, 1e-10);
	if exitflag == 1 % the LP problem is infeasible
        disp('The LP problem is infeasible')
        return
	end
	% create the basic and nonbasic lists
	[~, y1] = size(A);
	temp = n + 1:y1;
	for i = 1:length(temp)
        jb(length(jb) + 1) = temp(i);
	end
	BasicList = sort(jb);
	NonBasicList = setdiff(1:y1, BasicList);
	% delete redundant constraints found by lprref
	A(out, :) = [];
	b(out, :) = [];
	Eqin(out, :) = [];
end
flag = 0;
[m1, n1] = size(A); % new size of matrix A
% calculate the density of matrix A
density = (nnz(A) / (m1 * n1)) * 100;
% if the density of matrix A is less than 20%, then 
% use sparse algebra for faster computations
if density < 20
	A = sparse(A);
	c = sparse(c);
	b = sparse(b);
end
% preallocate memory for variables that hold large 
% amounts of data
Xb = spalloc(m1, 1, m1); % basic solution
h_l = spalloc(m1, 1, m1); % pivoting column
% the matrix of the basic variables
Basis = spalloc(m1, length(BasicList), m1 ...
    * length(BasicList));
% the matrix of the nonbasic variables
N = spalloc(m1, length(NonBasicList), m1 ...
    * length(NonBasicList));
w = spalloc(1, n1, n1); % simplex multiplier
Sn = spalloc(1, n1, n1); % reduced costs
% initialize data
% the matrix of the basic variables
Basis = A(:, BasicList);
% the coefficients of the objective function for the 
% basic variables
cb = c(BasicList);
% the matrix of the nonbasic variables
N = A(:, NonBasicList);
% the coefficients of the objective function for the 
% nonbasic variables
cn = c(NonBasicList);
% calculate the basis inverse
BasisInv = inv(Basis);
Xb = BasisInv * b; % basic solution
% set to zero, the values of Xb that are less than or
% equal to tole1
toler = abs(Xb) <= tole1;
Xb(toler == 1) = 0;
w = cb' * BasisInv; % calculate the simplex multiplier
% calculate the reduced costs
Sn = sparse(cn' - w * N);
% set to zero, the values of Sn that are less than or
% equal to tole2
toler = abs(Sn) <= tole2;
Sn(toler == 1) = 0;
% check if the current basis is dual feasible
if all(Sn >= 0) % the solution is dual feasible, skip big-M 
    % method and proceed to the dual simplex algorithm
    flag = 1;
else % the solution is not dual feasible, apply big-M method
    flag = 0;
end
counter = 1;
if flag == 0 % modified big-M method
    disp(['---- D U A L    W I T H    B I G - M    ' ...
        'M E T H O D ----'])
    % find the entering variable
	[p, t] = min(Sn);
	rr = find(Sn == p);
    % break the ties in order to avoid stalling
    if length(rr) > 1
        l = NonBasicList(rr);
        [l, ~] = max(l);
    else
        l = NonBasicList(t);
    end
    % add a constraint and an artificial variable
	A(m1 + 1, :) = sparse(1, n1);
	A(m1 + 1, NonBasicList) = 1;
	A = [A [sparse(m1, 1); 1]];
	c = [c; 0];
	b = [b; 0];
    % the right-hand side of the big-M method
	bM = [sparse(m1, 1); 1];
    % compute the new basic and nonbasic variables
	BasicList(m1 + 1) = l;
	NonBasicList(NonBasicList == l) = n1 + 1;
    % the artificial variable is in the nonbasic list
    artificialVariableInN = 1;
    Basis = A(:, BasicList); % the new Basis
    % the matrix of the nonbasic variables
	N = A(:, NonBasicList);
    % re-compute the inverse of the matrix
	BasisInv = inv(Basis);
    % compute the basic solution for the original problem
    % and the big-M problem
    Xb = BasisInv * b;
    % set to zero, the values of Xb that are less than or
    % equal to tole1
    toler = abs(Xb) <= tole1;
    Xb(toler == 1) = 0;
	XbM = BasisInv * bM;
    % set to zero, the values of XbM that are less than or
    % equal to tole1
    toler = abs(XbM) <= tole1;
    XbM(toler == 1) = 0;
    % compute the simplex multiplier
	w = c(BasicList)' * BasisInv;
	HRN = w * N;
	% set to zero, the values of HRN that are less than or
	% equal to tole2
	toler = abs(HRN) <= tole2;
	HRN(toler == 1) = 0;
	% compute the reduced costs
	Sn = c(NonBasicList)' - HRN;
	% set to zero, the values of Sn that are less than or
    % equal to tole2
    toler = abs(Sn) <= tole2;
    Sn(toler == 1) = 0;
    while flag == 0 % iterate in big-M method
        % optimality test
        if all(XbM > 0)
            col = find(BasicList == (n1 + 1), 1);
            if ~isempty(col)
                % if the artificial variable is in the 
                % basic list, the problem is optimal
                % calculate the value of the objective 
                % function
                if MinMaxLP == 1 % maximization
                    fval = full(-((c(BasicList))' * Xb ...
                        + c0));
                else % minimization
                    fval = full((c(BasicList))' * Xb ...
                        + c0);
                end
                exitflag = 0;
                xsol = Xb;
                iterations = iterations + 1;
                disp('The LP problem is optimal')
                return
            else
                % if the artificial variable is not in 
                % the basic list, check if the reduced 
                % cost of the artificial variable is 
                % equal to zero
                % compute the simplex multiplier
                w = c(BasicList)' * BasisInv;
                HRN = w * A;
                % set to zero, the values of HRN that are 
                % less than or equal to tole2
                toler = abs(HRN) <= tole2;
                HRN(toler == 1) = 0;
                % compute the reduced costs
                S = c' - HRN;
                % set to zero, the values of S that are 
                % less than or equal to tole2
                toler = abs(S) <= tole2;
                S(toler == 1) = 0;
                if S(n1 + 1) == 0 % if the reduced cost of 
                    % the artificial variable is equal to 
                    % zero, then the problem is optimal
                    % calculate the value of the objective 
                    % function
                    if MinMaxLP == 1 % maximization
                        fval = full(-((c(BasicList))' * Xb ...
                            + c0));
                    else % minimization
                        fval = full((c(BasicList))' * Xb ...
                            + c0);
                    end
                    exitflag = 0;
                    xsol = Xb;
                    iterations = iterations + 1;
                    disp('The LP problem is optimal')
                    return
                else % the problem is unbounded
                    disp('The LP problem is unbounded')
                    exitflag = 2;
                    iterations = iterations + 1;
                    return
                end
            end
        elseif all(XbM >= 0) % optimality test
            row = find(XbM == 0);
            if ~isempty(row)
                if all(Xb(row) >= 0)
                    col = find(BasicList == (n1 + 1));
                    if (~isempty(col) && XbM(col) == 0 && ...
                        Xb(col) == 0) || isempty(col)
                        % if the artificial variable is in 
                        % the basic list, check if the reduced 
                        % cost of the artificial variable is 
                        % equal to zero
                        % compute the simplex multiplier
                        w = c(BasicList)' * BasisInv;
                        HRN = w * A;
                        % set to zero, the values of HRN that 
                        % are less than or equal to tole2
                        toler = abs(HRN) <= tole2;
                        HRN(toler == 1) = 0;
                        % compute the reduced costs
                        S = c' - HRN;
                        % set to zero, the values of S that 
                        % are less than or equal to tole2
                        toler = abs(S) <= tole2;
                        S(toler == 1) = 0;
                        if S(n1 + 1) == 0 % if the reduced 
                            % cost of the artificial variable
                            % is equal to zero, then the 
                            % problem is optimal
                            % calculate the value of the 
                            % objective function
                            if MinMaxLP == 1 % maximization
                                fval = full(-((c(BasicList))' ...
                                    * Xb + c0));
                            else % minimization
                                fval = full((c(BasicList))' ...
                                    * Xb + c0);
                            end
                            exitflag = 0;
                            xsol = Xb;
                            iterations = iterations + 1;
                            disp('The LP problem is optimal')
                            return
                        else % the problem is unbounded
                            disp('The LP problem is unbounded')
                            exitflag = 2;
                            iterations = iterations + 1;
                            return
                        end
                    elseif (~isempty(col) && XbM(col) > 0) ...
                            || Xb(col) > 0
                        % the problem is optimal
                        % calculate the value of the objective 
                        % function
                        if MinMaxLP == 1 % maximization
                            fval = full(-((c(BasicList))' * Xb ...
                                + c0));
                        else % minimization
                            fval = full((c(BasicList))' * Xb ...
                                + c0);
                        end
                        exitflag = 0;
                        xsol = Xb;
                        iterations = iterations + 1;
                        disp('The LP problem is optimal')
                        return
                    end
                else % the artificial variable left the
                     % nonbasic list
                    artificialVariableInN = 0;
                end
            end
        end
        % if the artificial variable is in the nonbasic list,
        % use XbM to select the leaving variable
        if artificialVariableInN == 1
            % find the leaving variable
            mrt = find(XbM < 0);
            [a, r] = min(XbM(mrt));
            rr = find((XbM(mrt)) == a);
            % break the ties in order to avoid stalling
            if length(rr) > 1
                r = mrt(rr);
                % index of the leaving variable
                k = BasicList(r);
                [k, ~] = max(k);
                r = find(BasicList == k);
            else
                r = mrt(r);
                % index of the leaving variable
                k = BasicList(r);
            end
        else % if the artificial variable is not in the 
             % nonbasic list, use Xb to select the 
             % leaving variable
            mrt = Xb(row) < 0;
            mrt = row(mrt);
            [a, r] = min(Xb(mrt));
            rr = find((Xb(mrt)) == a);
            % break the ties in order to avoid stalling
            if length(rr) > 1
                r = mrt(rr);
                k = BasicList(r);
                [k, ~] = max(k);
                % index of the leaving variable
                r = find(BasicList == k);
            else
                r = mrt(r);
                % index of the leaving variable
                k = BasicList(r);
            end
        end
        % compute HRN vector
        HRN = BasisInv(r, :) * N;
        % set to zero, the values of HRN that are less than 
        % orequal to tole2
        toler = abs(HRN) <= tole2;
        HRN(toler == 1) = 0;
        mrt = find(HRN < 0);
        if isempty(mrt) % if there is not any candidate
            % to enter the basic list, then the problem
            % is infeasible
            exitflag = 1;
            disp('The problem is infeasible')
            return;
        end
        % perform the minimum ratio test to select
        % the entering variable
        [a, t] = min(-Sn(mrt) ./ HRN(mrt));
        rr = find(-Sn(mrt) ./ HRN(mrt) == a);
        % break the ties in order to avoid stalling
        if length(rr)>1
            l = NonBasicList(mrt(rr));
            % index of the entering variable
            [l, ~] = max(l);
        else
            % index of the entering variable
            l = NonBasicList(mrt(t));
        end
        % calculate the pivoting column
        h_l = BasisInv * A(:, l);
        % set to zero, the values of h_l that are less than 
        % or equal to tole3
        toler = abs(h_l) <= tole3;
        h_l(toler == 1) = 0;
        % check if the problem is unbounded
        if all(h_l <= 0)
            disp('The LP problem is unbounded')
            exitflag = 2;
            iterations = iterations + 1;
            return
        end
        % pivoting and update vectors and matrices
        f = Xb(r);
        fM = XbM(r);
        t = find(NonBasicList == l);
        g = h_l(r);
        v = Sn(t);
        % update the basic and the nonbasic lists
        BasicList(r) = l;
        NonBasicList(t) = k;
        % update the matrix of the nonbasic variables
        N = A(:, NonBasicList);
        iterations = iterations + 1;
        % calculate the new basic solution
        Xb(r) = 0;
        XbM(r) = 0;
        h_l2 = h_l;
        h_l2(r) = -1;
        if artificialVariableInN == 1 % the artificial 
            % variable is in the nonbasic list, so 
            % update XbM
            XbM = XbM - (fM / g) * h_l2;
            % set to zero, the values of XbM that are less 
            % than or equal to tole1
            toler = abs(XbM) <= tole1;
            XbM(toler == 1) = 0;
        end
        Xb = Xb - (f / g) * h_l2;
        % set to zero, the values of Xb that are less than 
        % or equal to tole1
        toler = abs(Xb) <= tole1;
        Xb(toler == 1) = 0;
        % update the reduced costs
        Sn(t) = 0; 
        HRN(t) = 1;
        Sn = Sn - (v / g) * HRN;
        % set to zero, the values of Sn that are less than 
        % or equal to tole2
        toler = abs(Sn) <= tole2;
        Sn(toler == 1) = 0;
        % basis inverse
        if iterations == counter * reinv
            % recompute the inverse of the basis 
            % from scratch every reinv iterations
            BasisInv = inv(A(:, BasicList));
            counter = counter + 1;
            h_l(r) = g;
        else
            % basis update according to the
            % selected basis update method
            if basisUpdateMethod == 1 % pfi
                BasisInv = pfi(BasisInv, h_l, r);
            else % mpfi
                BasisInv = mpfi(BasisInv, h_l, r);
            end
            h_l(r) = -1;
        end
        % print intermediate results every 100
        % iterations
        if mod(iterations, 100) == 0
            % calculate the value of the objective 
            % function
            if MinMaxLP == 1 % maximization
                fval = full(-(c(BasicList)' * Xb) + c0);
            else % minimization
                fval = full(c(BasicList)' * Xb + c0);
            end
            fprintf(['Iteration %i - objective value: ' ...
                '%f\n'], iterations, fval);
        end
    end
end
if flag == 1 % dual simplex method
    disp('---- D U A L ----')
    while flag == 1
        % optimality test
        if all(Xb >= 0) % the problem is optimal
            % calculate the value of the objective 
            % function
            if MinMaxLP == 1 % maximization
                fval = full(-((c(BasicList))' * Xb + c0));
            else % minimization
                fval = full((c(BasicList))' * Xb + c0);
            end
            exitflag = 0;
            xsol = Xb;
            iterations = iterations + 1;
            disp('The LP problem is optimal')
            return
        end
        % find the leaving variable
        mrt = find(Xb < 0);
        [a, r] = min(Xb(mrt));
        rr = find(Xb(mrt) == a);
        % break the ties in order to avoid stalling
        if length(rr) > 1
            r = mrt(rr);
            % index of the leaving variable
            k = BasicList(r);
            [k, ~] = max(k);
            r = find(BasicList == k);
        else
            r = mrt(r);
            % index of the leaving variable
            k = BasicList(r);
        end
        % compute HRN vector
        HRN = BasisInv(r, :) * N;
        % set to zero, the values of HRN that are less than 
        % or equal to tole3
        toler = abs(HRN) <= tole2;
        HRN(toler == 1) = 0;
        mrt = find(HRN < 0);
        if isempty(mrt) % if there is not any candidate
            % to enter the basic list, then the problem
            % is infeasible
            exitflag = 1;
            disp('The problem is infeasible')
            return;
        end
        % perform the minimum ratio test to select
        % the entering variable
        [a, t] = min(-Sn(mrt) ./ HRN(mrt));
        rr = find(-Sn(mrt) ./ HRN(mrt) == a);
        % break the ties in order to avoid stalling
        if length(rr)>1
            l = NonBasicList(mrt(rr));
            % index of the entering variable
            [l, ~] = max(l);
        else
            % index of the entering variable
            l = NonBasicList(mrt(t));
        end
        % calculate the pivoting column
        h_l = BasisInv * A(:, l);
        % set to zero, the values of h_l that are less than 
        % or equal to tole3
        toler = abs(h_l) <= tole3;
        h_l(toler == 1) = 0;
        % check if the problem is unbounded
        if all(h_l <= 0)
            disp('The LP problem is unbounded')
            exitflag = 2;
            iterations = iterations + 1;
            return
        end
        % pivoting and update vectors and matrices
        f = Xb(r);
        t = find(NonBasicList == l);
        g = h_l(r);
        % update the basic and the nonbasic lists
        BasicList(r) = l;
        NonBasicList(t) = k;
        % update the matrix of the nonbasic variables
        N = A(:, NonBasicList);
        iterations = iterations + 1;
        % calculate the new basic solution
        Xb(r) = 0;
        h_l2 = h_l;
        h_l2(r) = -1;
        Xb = Xb - (f / g) * h_l2;
        % set to zero, the values of Xb that are less than 
        % or equal to tole1
        toler = abs(Xb) <= tole1;
        Xb(toler == 1) = 0;
        % update the reduced costs
        Sn(t) = 0;
        HRN(t) = 1;
        Sn = Sn + a * HRN;
        % set to zero, the values of Sn that are less than 
        % or equal to tole2
        toler = abs(Sn) <= tole2;
        Sn(toler == 1) = 0;
        % basic inverse
        if iterations == counter * reinv
            % recompute the inverse of the basis 
            % from scratch every reinv iterations
            BasisInv = inv(A(:, BasicList));
            counter = counter + 1;
            h_l(r) = g;
        else
            % basis update according to the
            % selected basis update method
            if basisUpdateMethod == 1 % pfi
                BasisInv = pfi(BasisInv, h_l, r);
            else % mpfi
                BasisInv = mpfi(BasisInv, h_l, r);
            end
            h_l(r) = -1;
        end
        % print intermediate results every 100
        % iterations
        if mod(iterations, 100) == 0
            % calculate the value of the objective 
            % function
            if MinMaxLP == 1 % maximization
                fval = full(-(c(BasicList)' * Xb) + c0);
            else % minimization
                fval = full(c(BasicList)' * Xb + c0);
            end
            fprintf(['Iteration %i - objective value: ' ...
                '%f\n'], iterations, fval);
        end
    end
end
end