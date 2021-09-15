% sep_system_example.m
% This MATLAB program solves a linear system to compute the 
% mass flowrates in a simple separation system.
% K. Beers. MIT ChE. 8/4/03

function iflag_main = sep_system_example();
iflag_main = 0;

% set matrix of coefficients
A = zeros(3,3);
A(1,:) = [ 1 1 1];
A(2,:) = [0.04 0.54 0.26];
A(3,:) = [0.93 0.24 0];

% set right hand side vector
b = [10; 2; 6];

% solve for unknown and display results
x = A\b;
disp(' '); disp('solution x = '); disp(x');

iflag_main = 1;
return;
