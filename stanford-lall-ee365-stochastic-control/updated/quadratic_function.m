%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% class: quadratic function
% implement the subset of the calculus of quadratic functions
% needed for value iteration for a linear-quadratic stochastic
% control problem
%
% properties:
%    P : the quadratic coefficient
%    q : the linear coefficient
%    r : the constant coefficient
% methods:
%    qf = quadratic_function(P,q,r)
%    --> constructor
%    plus(qf1,qf2) ; alias: qf1 + qf2
%    --> add quadratic functions
%    mtimes(c,qf1) ; alias: c * qf1
%    --> product of a constant c and a quadratic function qf1
%    qf(x) ; qf(y,m) ; qf(lf)
%    --> evaluate a quadratic function qf at the point x
%    --> partial evaluation of a quadratic function qf
%        when the last m entries are equal to y
%    --> compose a quadratic function qf
%        with a linear function lf
%    [qfx , lfu] = min(qf , m)
%    --> compute the partial minimization of qf
%        over the last m entries
%    --> qfx is the minimum value
%        (a quadratic function of the other entries)
%    --> lfu is the minimizer
%        (a linear function of the other entries)
%    qf2 = partial_expectation(qf1 , ybar , yvar)
%    --> compute the partial expectation of qf1
%        with respect to the last length(ybar) entries
%        when those entries have mean ybar and variance yvar
%    disp(qf)
%    --> print a quadratic function qf to the console
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
classdef quadratic_function
    % define the class properties
    properties
        P
        q
        r
    end

    % define the class methods
    methods
        % constructor
        function qf = quadratic_function(P,q,r)
            if nargin > 0
                qf.P = P;
                qf.q = q;
                qf.r = r;
            end
        end

        % addition
        function qf3 = plus(qf1,qf2)
            qf3 = quadratic_plus_quadratic(qf1,qf2);
        end

        % scalar multiplication
        function qf2 = mtimes(c,qf1)
            qf2 = constant_times_quadratic(c,qf1);
        end

        % reference fields, evaluate at a point,
        % compose with linear function
        function qf2 = subsref(qf1,lf)
            switch lf.type
                % reference the fields of the linear function
                case '.'
                    switch lf.subs
                        case 'P'
                            qf2 = qf1.P;
                        case 'q'
                            qf2 = qf1.q;
                        case 'r'
                            qf2 = qf1.r;
                        otherwise
                            error('unsupported subreference')
                    end
                case '()'
                    if length(lf.subs) == 2
                        qf2 = partial_evaluation(qf1,lf.subs{1},lf.subs{2});
                        return;
                    end
                    
                    lf = lf.subs{1};
                    if isa(lf,'double')
                        % evaluate at a point
                        qf2 = evaluate_quadratic(qf1,lf);
                    elseif isa(lf,'linear_function')
                        % compose with linear function
                        qf2 = ...
                           quadratic_precompose_linear(qf1,lf);
                    else
                        error('unsupported subreference');
                    end
                otherwise
                    error('unsupported subreference');
            end
        end

        % minimize over the last m entries
        function [qfx , lfu] = min(qf,m)
            [qfx , lfu] = partial_minimization(qf , m);
        end
        
        % compute the partial expectation over tail entries
        function qf2 = Expect(qf1,ybar,yvar)
            qf2 = partial_expectation(qf1,ybar,yvar);
        end

        % print a representation to the console
        function disp(qf)
            disp('quadratic function:');
            if ~isempty(qf.P)
                disp('P =');
                disp(qf.P);
                disp('q =');
                disp(qf.q);
                disp('r =');
                disp(qf.r);
            end
        end
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% qf3 = quadratic_plus_quadratic(qf1,qf2)
% sum qf3 of quadratic functions qf1 and qf2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function qf3 = quadratic_plus_quadratic(qf1,qf2)
    % TODO
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% qf2 = constant_times_quadratic(c,qf1)
% product of the scalar c and the quadratic function qf1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function qf2 = constant_times_quadratic(c,qf1)
    % TODO
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% y = evaluate_quadratic(qf,x)
% evaluate the quadratic function qf at the point x
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function y = evaluate_quadratic(qf,x)
    y = (1/2) * (x' * qf.P * x) + qf.q' * x + (1/2) * (qf.r);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compute the composition of the quadratic function qf1
% and the linear function lf
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function qf2 = quadratic_precompose_linear(qf1,lf)
    % TODO
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% [qfx , lfu] = partial_minimization(qf,m)
% compute the partial minimization of the quadratic function qf
% over the last m entries
% --> qfx is the minimum value as a quadratic function
% --> lfu is the minimizer as a linear function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [qfx , lfu] = partial_minimization(qf,m)
    % TODO
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% qf2 = partial_evaluation(qf,y,m)
% compute the partial evaluation of the quadratic function
% qf with the last m entries of the argument equal to y
% --> qf2 is the value of qf as a quadratic function of the
%     remaining variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function qf2 = partial_evaluation(qf,y,m)
    % TODO
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% qf2 = partial_expectation(qf1,ybar,yvar)
% compute the partial expectation of the quadratic function
% qf1 with respect to the last length(ybar) entries,
% when those entries have mean ybar, and variance yvar
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function qf2 = partial_expectation(qf1,ybar,yvar)
    % TODO
end
