function [xsol, fval, exitflag, iterations] = ...
    epsa(A, c, b, Eqin, MinMaxLP, c0, reinv, tole1, ...
    tole2, tole3, scalingTechnique, basisUpdateMethod)
% Filename: epsa.m
% Description: the function is an implementation of the 
% primal exterior point simplex algorithm
% Authors: Ploskas, N., Samaras, N., Triantafyllidis, Ch.
%
% Syntax: [xsol, fval, exitflag, iterations] = ...
%   epsa(A, c, b, Eqin, MinMaxLP, c0, reinv, tole1, ...
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
% -- tole3: tolerance for the pivot column (optional: 
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
elseif scalingTechnique == 7 % geometricMean
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
h_l = spalloc(m1, 1, m1); % pivot column
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
w = cb' * BasisInv; % simplex multiplier
Sn = sparse(cn' - w * N); % reduced costs
% set to zero, the values of Xb that are less than or
% equal to tole2
toler = abs(Sn) <= tole2;
Sn(toler == 1) = 0;
% check if the direction crosses the feasible region 
% to avoid Phase I
P = NonBasicList(find(Sn < 0));
if ~isempty(P)
    h_j = BasisInv * A(:, P);
    dB = -(sum(h_j, 2));
    % set to zero, the values of dB that are less than 
    % or equal to tole3
    toler = (abs(dB)) <= tole3;
    dB(toler == 1) = 0;
    % calculate ratio a
    mrtarat = find(dB < 0);
    if isempty(mrtarat)
        arat = inf;
    else
        mrtarat2 = find(-dB([mrtarat]) ~= 0);
        mrtarat = mrtarat(mrtarat2);
        aratios = Xb([mrtarat]) ./ (-dB([mrtarat]));
        % set to zero, the values of aratios that are less 
        % than or equal to tole1
        toler = (abs(aratios)) <= tole1;
        aratios(toler==1) = 0;
        arat = min(aratios);
    end
    % calculate ratio b
    mrtbrat = find(Xb < 0);
    if isempty(mrtbrat)
        brat = -inf;
    else
        mrtbrat2 =find(-dB([mrtbrat]) ~=0 );
        mrtbrat = mrtbrat(mrtbrat2);
        bratios = Xb([mrtbrat]) ./ (-dB([mrtbrat]));
        % set to zero, the values of bratios that are less 
        % than or equal to tole1
        toler = (abs(bratios)) <= tole1;
        bratios(toler==1) = 0;
        idinf = find(bratios == -inf);
        if ~isempty(idinf)
            brat = inf;
        else
            brat = max(bratios);
        end
    end
end
if isempty(P)
    flag = 0;
elseif brat < arat % proceed to Phase II
    flag = 1;
else
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
	% re-define matrix A since artificial variable needs 
	% to be added for fase 1
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
                                % nonbasic list
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
            % find the entering variable using Dantzig's
            % rule
            [~, t] = min(Sn);
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
                % avoid stalling because of ties
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
if flag == 1 %  Phase II
	disp('---- P H A S E    II ----')
	% matrix A does not contain the artificial variable.
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
    % compute set P
    P = NonBasicList(Sn < 0);
    % compute set Q
    Q = NonBasicList(Sn >= 0);
    % find direction dB
    h_j = BasisInv * A(:, P);
    dB = -(sum(h_j, 2));
    % find vector SP
    SP = c(P)' - w * A(:, P);
    % set to zero, the values of SP that are less than or
	% equal to tole2
    toler = abs(SP) <= tole2;
    SP(toler == 1) = 0;
    % find vector SQ
    SQ = c(Q)' - w * A(:, Q);
    % set to zero, the values of SQ that are less than or
	% equal to tole2
    toler = abs(SQ) <= tole2;
    SQ(toler == 1) = 0;
    while flagfase2 == 0 % iterate in Phase II
        flag = 0;
        % optimality test for Phase II
        if isempty(P) % the problem is optimal
            % calculate the value of the objective function
            if MinMaxLP == 1 % maximization
                fval = full(-((c(BasicList))' * Xb ...
                    - c0));
            else % minimization
                fval = full((c(BasicList))' * Xb ...
                    - c0);
            end
            exitflag = 0;
            xsol = Xb;
            disp('The LP problem is optimal')
            iterations = iterations + 1;
            return
        end
        if all(dB >= 0)
            so = sum(SP);
            if so == 0 % the problem is optimal
                % calculate the value of the objective function
                if MinMaxLP == 1 % maximization
                    fval = full(-((c(BasicList))' * Xb ...
                    - c0));
                else % minimization
                    fval = full((c(BasicList))' * Xb ...
                    - c0);
                end
                exitflag = 0;
                xsol = Xb;
                iterations = iterations + 1;
                disp('The LP problem is optimal')
                return
            elseif so < 0 % the problem is unbounded
                disp('The LP problem is unbounded')
                exitflag = 2;
                iterations = iterations + 1;
                return
            end
        end
        % find the leaving variable
        mrt = find(dB < 0);
        Xb_dB_div = Xb(mrt) ./ (-dB(mrt));
        [p, r] = min(Xb_dB_div);
        if p == +Inf % the problem is unbounded
            disp('The LP problem is unbounded')
            exitflag = 2;
            iterations = iterations + 1;
            return
        end
        rr = find(Xb_dB_div == p);
        if length(rr) > 1 % avoid stalling because of ties
            r = mrt(rr);
            % index of the leaving variable
            k = BasicList(r);
            [k, ~] = min(k);
            r = find(BasicList == k);
        else
            r = mrt(r);
            % index of the leaving variable
            k = BasicList(r);
        end
        % find the entering variable
        HrP = BasisInv(r, :) * A(:, P);
        % set to zero, the values of HrP that are 
        % less than or equal to tole3
        toler = abs(HrP) <= tole3;
        HrP(toler == 1) = 0;
        HrQ = BasisInv(r, :) * A(:, Q);
        % set to zero, the values of HrQ that are 
        % less than or equal to tole3
        toler = abs(HrQ) <= tole3;
        HrQ(toler == 1) = 0;
        mrtP = find(HrP > 0);
        mrtQ = find(HrQ < 0);
        % find theta1
        if ~isempty(mrtP)
            Ratios1 = (-SP(mrtP)) ./ (HrP(mrtP));
            % set to zero, the values of Ratios1 that 
            % are less than or equal to tole3
            toler = abs(Ratios1) <= tole3;
            Ratios1(toler == 1) = 0;
            % avoid ties
            [Rat1, ~] = min(Ratios1);
            t1 = mrtP(Ratios1 == Rat1);
            m_i = P(t1);
            for i = 1:length(m_i)
                indexes(i) = (find(P == m_i(i)));
            end
            t1 = min(indexes);
            clear indexes;
            clear m_i;
        else
            Rat1 = Inf;
        end
        % find theta2
        if ~isempty(mrtQ)
            Ratios2 = (-SQ(mrtQ)) ./ (HrQ(mrtQ));
            % set to zero, the values of Ratios2 that 
            % are less than or equal to tole3
            toler = abs(Ratios2) <= tole3;
            Ratios2(toler == 1) = 0;
            % avoid ties
            [Rat2, ~] = min(Ratios2);
            t2 = mrtQ(Ratios2 == Rat2);
            m_i = Q(t2);
            for i = 1:length(m_i)
                indexes(i) = (find(Q == m_i(i)));
            end
            t2 = min(indexes);
            clear indexes;
            clear m_i;
        else
            Rat2 = Inf;
        end
        % compare theta1 and theta2 to find the 
        % entering variable
        if Rat1 <= Rat2
            l = P(t1);
            P(t1) = [];
            Q = [Q k];
            flag = 1;
        else
            l = Q(t2);
            Q(t2) = k;
        end
        % pivoting and update vectors and matrices
        h_l = BasisInv * A(:, l); % pivoting column
        g = h_l(r); % the pivot element
        % update the basic list
        BasicList(r) = l;
        % compute dB
        EInv = speye(length(BasisInv));
        EInv(:, r) = -h_l(1:length(h_l)) / g;
        EInv(r, r) = 1 / g;
        dB = EInv * dB;
        clear EInv;
        if flag == 1
            dB(r) = dB(r) + 1;
        end
        % set to zero, the values of dB that are less 
        % than or equal to tole1
        toler = abs(dB) <= tole1;
        dB(toler == 1) = 0;
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
        % calculate the new basic solution
        Xb = BasisInv * b;
        % set to zero, the values of Xb that are less 
        % than or equal to tole1
        toler = abs(Xb) <= tole1;
        Xb(toler == 1) = 0;
        % update the nonbasic list
        NonBasicList = [P Q];
        iterations = iterations + 1;
        % calculate the coefficients of the objective 
        % function for the basic variables
        cb = c(BasicList);
        % calculate the simplex multiplier
        w = cb' * BasisInv;
        % calculate vectors SP and SQ
        SP = c(P)' - w * A(:, P);
        % set to zero, the values of SP that are less 
        % than or equal to tole2
        toler = abs(SP) <= tole2;
        SP(toler == 1) = 0;
        SQ = c(Q)' - w * A(:, Q);
        % set to zero, the values of SQ that are less 
        % than or equal to tole2
        toler = abs(SQ) <= tole2;
        SQ(toler == 1) = 0;
        % print intermediate results every 100
        % iterations
        if mod(iterations, 100) == 0
            % calculate the value of the objective 
            % function
            if MinMaxLP == 1 % maximization
                fval = full(-(c(BasicList)' * Xb - c0));
            else % minimization
                fval = full(c(BasicList)' * Xb - c0);
            end
            fprintf(['Iteration %i - ' ...
                'objective value: %f\n'], ...
                iterations, fval);
        end
    end
end
end