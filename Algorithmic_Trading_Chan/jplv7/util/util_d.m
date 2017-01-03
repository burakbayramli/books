% PURPOSE: demonstrate some of the utility functions
%
%
% usage: util_d

clear all;

% demonstrate vec
x = 1:20;
mat = [x(1,1:10)' x(1,11:20)'];

v = vec(mat);

disp('matrix =');
mat

disp('stacked vector =');
v

% demonstrate p_latex
numbers = rand(10,4);
disp('LaTeX formatted table from matrix');
lprint(numbers);

% now use a different format
disp('different user-supplied format for table from matrix');
in.fmt = strvcat('%12.4f');
lprint(numbers,in);


% demonstrate accumulate
acc = accumulate(mat);
disp('matrix columns accumulated');
acc

% demonstrate seasonal dummy variables
seas = sdummy(36,12);
disp('seasonal dummy variables');
seas
