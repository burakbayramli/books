function [xsol, fval, exitflag, iterations] = ...
    rsa(A, c, b, Eqin, MinMaxLP, c0, reinv, tole1, ...
    tole2, tole3, scalingTechnique, pivotingRule, ...
    basisUpdateMethod)
% Filename: rsa.m
% Description: the function is an implementation of the 
% revised primal simplex algorithm
% Authors: Ploskas, N., & Samaras, N.
%
% Syntax: [xsol, fval, exitflag, iterations] = ...
%   rsa(A, c, b, Eqin, MinMaxLP, c0, reinv, tole1, ...
%   tole2, tole3, scalingTechnique, pivotingRule, ...
%   basisUpdateMethod)
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
% -- pivotingRule: the pivoting rule to be used
%    (1: Bland's rule, 2: Dantzig's rule, 3: Greatest
%    Increment Method, 4: Least Recently Considered 
%    Method, 5: Partial Pricing Rule, 6: Steepest Edge
%    Rule) (optional: default value 2)
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
if ~exist('pivotingRule')
	pivotingRule = 2;
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
	A(:, n + 1:n + axm) = zeros(m, axm);
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
optTest = spalloc(m1, 1, m1); % optimality test vector
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
% check if the Phase I is needed
if all(Xb >= 0) % the solution is feasible, skip Phase I
	flag = 1;   % and proceed to Phase II
else % the solution is not feasible, proceed to Phase I
	flag = 0;
end
counter = 1;
if flag == 0 % Phase I
	disp('---- P H A S E    I ----')
	% find the index of the minimum element of vector Xb
	[~, minindex] = min(Xb);
	% create a copy of vector c for the Phase I
	c2 = sparse(zeros(length(c), 1));
	c2(length(c) + 1) = 1;
	% calculate vector d
	d = sparse(-(Basis) * ones(m1, 1));
	% re-define matrix A since the artificial variable 
	% needs to be added for Phase I
	A(:, n1 + 1) = d;
	% compute the new basic and nonbasic variables
	NonBasicList(1, length(NonBasicList) + 1) = ...
        BasicList(1, minindex);
	BasicList(:, minindex) = n1 + 1;
	[~, n] = size(A); % the new size of matrix A
	flagfase1 = 0;
	Basis = A(:, BasicList); % the new basis
	% the new coefficients of the objective function for 
	% the basic variables
	fb = c2(BasicList);
	% the matrix of the nonbasic variables
	N = A(:, NonBasicList);
	% the new coefficients of the objective function for 
	% the nonbasic variables
	fn = c2(NonBasicList);
	% re-compute the inverse of the matrix
	BasisInv = inv(Basis);
	% compute the basic solution
	Xb = BasisInv * b;
	% set to zero, the values of Xb that are less than or
	% equal to tole1
	toler = abs(Xb) <= tole1;
	Xb(toler == 1) = 0;
	% compute the simplex multiplier
	w = fb' * BasisInv;
	HRN = w * N;
	% set to zero, the values of HRN that are less than or
	% equal to tole2
	toler = abs(HRN) <= tole2;
	HRN(toler == 1) = 0;
	% compute the reduced costs
	Sn = fn' - HRN;
	% set to zero, the values of Sn that are less than or
	% equal to tole2
	toler = abs(Sn) <= tole2;
	Sn(toler == 1) = 0;
	while flagfase1 == 0 % iterate in Phase I
        clear rr;
        clear mrt;
        clear h_l;
        [~, col] = find(NonBasicList == n);
        % optimality test for Phase I
        if all(Sn >= 0) || ~isempty(col)
            % if the artificial variable is leaving from 
            % the basic list, Phase I must be terminated 
            % and Phase II must start.
            % The basis produced is feasible for Phase II
            if ~isempty(col)
                NonBasicList(col) = [];
                A(:, n) = [];
                flag = 1;
                iterations = iterations + 1;
                break
            else
                % If the artificial variable is in the 
                % basic list but Sn >= 0, the artificial 
                % variable is leaving and as an entering 
                % one, we pick a random but acceptable 
                % variable.
                % The basis produced is feasible for 
                % Phase II
                [~, col] = find(BasicList == n);
                if ~isempty(col)
                    if Xb(col) == 0
                        A(:, n) = [];
                        for i = 1:length(NonBasicList)
                            l = NonBasicList(i);
                            h_l = BasisInv * A(:, l);
                            % set to zero, the values of 
                            % h_l that are less than or
                            % equal to tole3
                            toler = abs(h_l) <= tole3;
                            h_l(toler == 1) = 0;
                            if h_l(col) ~= 0
                                % update the basic and 
                                % nonbasic lists
                                BasicList(col) = l;
                                NonBasicList(i) = [];
                                % re-compute the basis
                                % inverse
                                BasisInv = inv(A(:, ...
                                    BasicList));
                                % re-compute the
                                % reduced costs
                                Sn = c(NonBasicList)' - ...
                                    c(BasicList)' * BasisInv ...
                                    * A(:, NonBasicList);
                                % set to zero, the values of 
                                % Sn that are less than or
                                % equal to tole2
                                toler = abs(Sn) <= tole2;
                                Sn(toler == 1) = 0;
                                flag = 1;
                                iterations = iterations + 1;
                                break
                            end
                        end
                        break
                    else % the LP problem is infeasible
                        disp('The LP problem is infeasible')
                        exitflag = 1;
                        iterations = iterations + 1;
                        return
                    end
                end
            end
        else % the LP problem of Phase I is not optimal
            % find the entering variable using the selected
            % pivoting rule
            if pivotingRule == 1 % bland
                t = bland(Sn, NonBasicList);
            elseif pivotingRule == 2 % dantzig
                t = dantzig(Sn);
            % greatest increment
            elseif pivotingRule == 3
                t = gim(Sn, NonBasicList, A, ...
                    BasisInv, Xb, tole3);
            % least recently considered
            elseif pivotingRule == 4
                if ~exist('ind') % first time, start at zero
                    t = lrcm(Sn, NonBasicList, 0, ...
                        iterations);
                else % continue from the last found index
                    t = lrcm(Sn, NonBasicList, ...
                        lrcmLast, iterations);
                end
                lrcmLast = t;
            elseif pivotingRule == 5 % partial pricing
                % select a segment size according to the
                % size of the problem
                if length(Sn) <= 500
                    segmentSize = 20;
                elseif length(Sn) > 1000
                    segmentSize = 50;
                else
                    segmentSize = 35;
                end
                t = partialPricing(Sn, segmentSize);
            elseif pivotingRule == 6 % steepest edge
                t = steepestEdge(Sn, NonBasicList, ...
                    A, BasisInv);
            end
            % index of the entering variable
            l = NonBasicList(t);
            % calculate the pivoting column
            h_l = BasisInv * A(:, l);
            % set to zero, the values of h_l that are 
            % less than or equal to tole3
            toler = abs(h_l) <= tole3;
            h_l(toler == 1) = 0;
            optTest = h_l > 0;
            % perform the minimum ratio test to find 
            % the leaving variable
            if sum(optTest) > 0
                mrt = find(h_l > 0);
                Xb_hl_div = Xb(mrt) ./ h_l(mrt);
                [p, r] = min(Xb_hl_div);
                rr = find(Xb_hl_div == p);
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
                % pivoting and update vectors and matrices
                a = Sn(t);
                f = Xb(r);
                % update the basic list
                BasicList(r) = l;
                g = h_l(r); % the pivot element
                % update the nonbasic list
                NonBasicList(t) = k;
                % update the matrix of the nonbasic variables
                N = A(:, NonBasicList);
                iterations = iterations + 1;
                % calculate the new basic solution
                Xb(r) = 0;
                h_l2 = h_l;
                h_l2(r) = -1;
                Xb = Xb - (f / g) * h_l2;
                % set to zero, the values of Xb that 
                % are less than or equal to tole1
                toler = abs(Xb) <= tole1;
                Xb(toler == 1) = 0;
                % update the reduced costs
                Sn(t) = 0;
                HRN = BasisInv(r, :) * N;
                % set to zero, the values of HRN that 
                % are less than or equal to tole2
                toler = abs(HRN) <= tole2;
                HRN(toler == 1) = 0;
                Sn = Sn - (a / g) * HRN;
                % set to zero, the values of Sn that 
                % are less than or equal to tole2
                toler = abs(Sn) <= tole2;
                Sn(toler == 1) = 0;
                % basis inverse
                if iterations == counter * reinv
                    % recompute the inverse of the basis 
                    % from scratch every reinv iterations
                    BasisInv = inv(A(:, BasicList));
                    counter = counter + 1;
                    h_l(r) = g;
                else % basis update according to the 
                    % selected basis update method
                    if basisUpdateMethod == 1 % pfi
                        BasisInv = pfi(BasisInv, h_l, r);
                    else % mpfi
                        BasisInv = mpfi(BasisInv, h_l, r);
                    end
                    h_l(r) = -1;
                end
            end
        end
	end
end
if flag == 1 % Phase II
	disp('---- P H A S E    II ----')
	% matrix A does not contain now the artificial variable.
	% Here we solve the original LP problem
	flagfase2 = 0;
	[m1, n1] = size(A); % new size of matrix A
	% calculate the needed variables for the algorithm to 
	% begin with the matrix of the basic variables
	Basis = spalloc(m1, length(BasicList), m1 * ...
        length(BasicList));
	% the matrix of the nonbasic variables
	N = spalloc(m1, length(NonBasicList), m1 * ...
        length(NonBasicList));
	w = spalloc(1, n1, n1); % simplex multiplier
	Sn = spalloc(1, n1, n1); % reduced costs
	Basis = A(:, BasicList); % the new basis
	% the coefficients of the objective function for the 
	% basic variables
	cb = c(BasicList);
	% the matrix of the basic variables
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
	% calculate the simplex multiplier
	w = cb' * BasisInv;
	HRN = w * N;
	% set to zero, the values of HRN that are less than or
	% equal to tole2
	toler = abs(HRN) <= tole2;
	HRN(toler == 1) = 0;
	% calculate the reduced costs
	Sn = cn' - HRN;
	% set to zero, the values of Sn that are less than or
	% equal to tole2
	toler = abs(Sn) <= tole2;
	Sn(toler == 1) = 0;
	while flagfase2 == 0 % iterate in Phase II
        clear optTest
        clear rr;
        clear mrt;
        clear h_l;
        % optimality test for Phase II
        if ~all(Sn >= 0) == 1
            % find the entering variable using the 
            % selected pivoting rule
            if pivotingRule == 1 % bland
                t = bland(Sn, NonBasicList);
            elseif pivotingRule == 2 % dantzig
                t = dantzig(Sn);
            % greatest increment
            elseif pivotingRule == 3
                t = gim(Sn, NonBasicList, A, ...
                    BasisInv, Xb, tole3);
            % least recently considered
            elseif pivotingRule == 4
                if ~exist('ind') % first time, start at zero
                    t = lrcm(Sn, NonBasicList, 0, ...
                        iterations);
                else % continue from the last found index
                    t = lrcm(Sn, NonBasicList, ...
                        lrcmLast, iterations);
                end
                lrcmLast = t;
            elseif pivotingRule == 5 % partial pricing
                % select a segment size according to the
                % size of the problem
                if length(Sn) <= 500
                    segmentSize = 20;
                elseif length(Sn) > 1000
                    segmentSize = 50;
                else
                    segmentSize = 35;
                end
                t = partialPricing(Sn, segmentSize);
            elseif pivotingRule == 6 % steepest edge
                t = steepestEdge(Sn, NonBasicList, ...
                    A, BasisInv);
            end
            % index of the entering variable
            l = NonBasicList(t);
            % calculate the pivoting column
            h_l = BasisInv * A(:, l);
            % set to zero, the values of h_l that are 
            % less than or equal to tole3
            toler = abs(h_l) <= tole3;
            h_l(toler == 1) = 0;
            optTest = h_l > 0;
            % perform the minimum ratio test to find the 
            % leaving variable
            if sum(optTest) > 0
                mrt = find(h_l > 0);
                Xb_hl_div = Xb(mrt) ./ h_l(mrt);
                [p, r] = min(Xb_hl_div);
                rr = find(Xb_hl_div == p);
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
                % pivoting and update vectors and 
                % matrices
                a = Sn(t);
                f = Xb(r);
                % update the basic list
                BasicList(r) = l;
                g = h_l(r); % the pivot element
                % update the nonbasic list
                NonBasicList(t) = k;
                % update the matrix of the nonbasic 
                % variables
                N = A(:, NonBasicList);
                iterations = iterations + 1;
                % calculate the new basic solution
                Xb(r) = 0;
                h_l2 = h_l;
                h_l2(r) = -1;
                Xb = Xb - (f / g) * h_l2;
                % set to zero, the values of Xb that 
                % are less than or equal to tole1
                toler = abs(Xb) <= tole1;
                Xb(toler == 1) = 0;
                % update the reduced costs
                Sn(t) = 0;
                HRN = BasisInv(r, :) * N;
                % set to zero, the values of HRN that 
                % are less than or equal to tole2
                toler = abs(HRN) <= tole2;
                HRN(toler == 1) = 0;
                Sn = Sn - (a / g) * HRN;
                % set to zero, the values of Sn that 
                % are less than or equal to tole2
                toler = abs(Sn) <= tole2;
                Sn(toler == 1) = 0;
                % basis inverse
                if iterations == counter * reinv
                    BasisInv = inv(A(:, BasicList));
                    counter = counter + 1;
                    h_l(r) = g;
                else % basis update according to the 
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
                        fval = full(-(c(BasicList)' * Xb) ...
                            + c0);
                    else % minimization
                        fval = full(c(BasicList)' * Xb ...
                            + c0);
                    end
                    fprintf(['Iteration %i - ' ...
                        'objective value: %f\n'], ...
                        iterations, fval);
                end
            else % the problem is unbounded
                disp('The LP problem is unbounded')
                exitflag = 2;
                iterations = iterations + 1;
                return
            end
        else % the problem is optimal
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
	end
end
end