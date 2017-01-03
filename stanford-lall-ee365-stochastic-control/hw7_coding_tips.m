%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using global variables in your functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% crossbar_switch_data defines all of the problem data as
% global variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this is so you can decompose your code into functions, and
% not have to worry about passing the data to each of your
% functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% an example of how to use a global variable within a function
% is shown below (note that changes to global variables will
% carry over to the base workspace)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [output_arguments] = my_function(input_arguments)
    global lambda
    % now you can use the lambda defined in crossbar_switch_data
    % (assuming you have already run crossbar_switch_data)
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% one way of formulating the routing problem represents the
% state x and the action u has m x n matrices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the computations are a lot easier if you come up with a
% vector representation of these matrices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% additionally, you may find it useful to represent x(i,j) and
% u(i,j) as linear functions of the vector [x(:) ; u(:)], so
% that your linear and quadratic function classes can handle
% most of the algebra for you
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the following code constructs cell arrays of linear funtions
% where, for example, xlf{i,j} is a representation of x(i,j)
% as a linear function of the vector [x(:) ; u(:)]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Ixu = eye(2*m*n);
xlf = cell(m,n);
ulf = cell(m,n);
for i = 1:m
    for j = 1:n
        k = sub2ind([m n] , i , j);
        xlf{i,j} = linear_function(Ixu(k,:),0);
        ulf{i,j} = linear_function(Ixu(m*n+k,:),0);
    end
end