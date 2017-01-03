function linefit(t,b)
%LINEFIT Plot the least squares fit by a line.
%	LINEFIT(t,b), where t and b are vectors of the same length,
%	displays the best line fit to the data points (t(i),b(i)).

% We now insure that t and b are column vectors.
t = t(:); b = b(:);

% Form the matrix whose first column is all ones and
% whose second column is the vector t.
n = length(t);
e = ones(n,1);
A = [e t];

% Solve the least squares problem, A*x ~= b.
xbar = lsq(A,b);
c = xbar(1);
d = xbar(2);

% Plot the results.
tline = [1.1*min(t)-0.1*max(t), 1.1*max(t)-0.1*min(t)];
yline = c + d*tline;
plot(t,b,'bo',t,c+d*t,'g*',tline,yline,'k-')
if d >= 0 , sign = ' + '; else, sign = ' - '; end
title(['Best line is ' num2str(c) sign num2str(abs(d)) '*t'])
xlabel('t')
