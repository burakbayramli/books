% test_Gershorgin.m
%
% function iflag_main = test_Gershorgin(A);
%
% This MATLAB m-file tests Gershorgin's theorem
% for an input real square matrix A.
%
% K. Beers. MIT ChE. 6/27/2002
function iflag_main = test_Gershorgin(A);
iflag_main = 0;

if(size(A,1) ~= size(A,2))
    error('A is not square');
end

% First, make the figure for the complex
% domain, and add the points for the diagonal
% components.
figure;
hold on;
for k=1:size(A,1)
    plot(A(k,k),0,'o');
end

% Now, add the circles for the off-diagonal elements.
theta = linspace(0,2*pi,100);
theta_cos = cos(theta);
theta_sin = sin(theta);
for k=1:size(A,1)
    row_k_abs = abs(A(k,:));
    rk = sum(row_k_abs) - abs(A(k,k));
    x = A(k,k) + rk*theta_cos;
    y = rk*theta_sin;
    plot(x,y,'--');
end
axis equal;
xlabel('Re(\lambda)');
ylabel('Im(\lambda)');
title('Test of Gershorgin''s theorem');
 
% Now, calculate the eigenvalues.
iflag_main = 1;
eig_val = eig(A);
for k=1:length(eig_val)
    plot(real(eig_val(k)),imag(eig_val(k)),'d');
end
gtext('O are diagonal elements, diamonds are eigenvalues');

return;
