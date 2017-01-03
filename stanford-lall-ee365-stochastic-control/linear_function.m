%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% class: linear_function
% implement the subset of the calculus of linear functions
% needed for value iteration for a linear-quadratic stochastic
% control problem
%
% properties:
%    A : the linear coefficient
%    b : the offset
% methods:
%    lf = linear_function(A,b)
%    --> constructor
%    plus(lf1,lf2) ; alias: lf1 + lf2
%    --> add linear functions
%    mtimes(c,lf1) ; alias: c * lf1
%    --> product of a constant c and a linear function lf1
%    lf(x) ; lf1(lf2)
%    --> evaluate the linear function lf at the point x
%    --> compose the linear function lf1
%        with the linear function lf2
%    disp(lf)
%    --> print a linear function lf to the console
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
classdef linear_function
    % define the class properties
    properties
        A
        b
    end

    % define the class methods
    methods
        % constructor
        function lf = linear_function(A,b)
            if nargin > 0
                lf.A = A;
                lf.b = b;
            end
        end

        % addition
        function lf3 = plus(lf1,lf2)
            lf3 = linear_plus_linear(lf1,lf2);
        end

        % multiplication
        function lf2 = mtimes(c,lf1)
            lf2 = constant_times_linear(c,lf1);
        end
        
        % reference fields, evaluate at a point, compose
        % linear functions
        function y = subsref(lf,x)
            switch x.type
                % reference the fields of the linear function
                case '.'
                    switch x.subs
                        case 'A'
                            y = lf.A;
                        case 'b'
                            y = lf.b;
                        otherwise
                            error('unsupported subreference');
                    end
                % evaluate a linear function at a point
                case '()'
                    x = x.subs{1};
                    if isa(x,'double')
                        y = evaluate_linear(lf,x);
                    elseif isa(x,'linear_function')
                        y = linear_compose_linear(lf,x);
                    else
                        error('unsupported subreference');
                    end
                otherwise
                    error('unsupported subreference');
            end
        end

        % print a representation to the console
        function disp(lf)
            disp('linear function');
            if ~isempty(lf.A)
                disp('A =');
                disp(lf.A);
                disp('b =');
                disp(lf.b);
            end
        end
    end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lf3 = linear_plus_linear(lf1,lf2)
% compute the sum lf3 of linear functions lf1 and lf2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lf3 = linear_plus_linear(lf1,lf2)
    lf3 = linear_function(lf1.A + lf2.A , lf1.b + lf2.b);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lf2 = constant_times_linear(c,lf1)
% product of the scalar c and the linear function lf1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lf2 = constant_times_linear(c,lf1)
    lf2 = linear_function(c * lf1.A , c * lf1.b);
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% y = evaluate_linear(lf,x)
% evaluate the linear function lf at the point x
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function y = evaluate_linear(lf,x)
    y = lf.A * x + lf.b;
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lf3 = linear_compose_linear(lf1,lf2)
% compose two linear functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function lf3 = linear_compose_linear(lf1,lf2)
    lf3 = linear_function(lf1.A * lf2.A , ...
                          lf1.b + lf1.A * lf2.b);
end