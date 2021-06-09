function demoWiggle
% demoWiggle  Plot wiggle from increasing order of polynomial interpolant
%             of data obtained by combining  two linear functions
%                 y = c1*x + c2;   y = c3*x + c4
%             There is a modest step where these two lines join
%
% Synopsis:  demoWiggle
%
% Input:     none
%
% Output:    Plots of polynomial interpolant, extrapolation and original
%            data for increasing order of polynomial

c1 = -0.5;   c2 = 4;          %  slope and intercept of lines used
c3 = -0.4;   c4 = c2 - 4;     %  to generate "known" data

n = 10;  x = 1:n;             %  "known" data
y = zeros(size(x));           %  preallocate y

mid = floor(n/2);
y(1:mid)   = c1*x(1:mid)   + c2;     %  line for first half of y data
y(mid+1:n) = c3*x(mid+1:n) + c4;     %  line for second half of y data

fprintf('\nSupport Points:\n');
fprintf('  x =');  fprintf(' %6.2f',x);  fprintf('\n');
fprintf('  y =');  fprintf(' %6.2f',y);  fprintf('\n\n');

% --- Indices of subsets of input data for interpolation; only for n=4!
istart = mid - [1 1 2 2 3 3 4  4];   %  start of data sets
nint   =       [3 4 5 6 7 8 9 10];   %  # points in interp set = order+1
minxex = 0;  maxxex = 11;            %  min & max values of extrapolated x

for k=1:length(nint)
  % --- Interpolate over range of input data subset
  is = istart(k);                    %  start index of data subset
  ie = is + nint(k) - 1;             %  end index of data subset
  xi = linspace(x(is),x(ie));
  yi = newtint(x(is:ie),y(is:ie),xi);
  % --- Extrapolate to left and right of input data subset
  xexleft = linspace(minxex,x(is));
  yexleft = newtint(x(is:ie),y(is:ie),xexleft);
  xexright = linspace(x(ie),maxxex);
  yexright = newtint(x(is:ie),y(is:ie),xexright);
  subplot(4,2,k);
  plot(x,y,'bo',xi,yi,'b-',xexleft,yexleft,'r--',xexright,yexright,'r--');
  axis([0 11 -6  6]);   %  enforce this axis scaling on all subplots
end
