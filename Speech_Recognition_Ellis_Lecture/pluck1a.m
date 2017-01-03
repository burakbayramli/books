function y = pluck1a(len,d,r,x,viz)
% y = pluck1a(l,d,r,x,v) Plucked string synthesis via pair of waveguide 'rails'
%    l is the length of each waveguide in samples.  
%    d is the number of output samples to produce (default 10000).
%    r is the refelection FIR at the far end (default [-.25 -.5 -.25] if empty)
%    x is optional initial shape to plucked string (triangular by default)
%      if x is a scalar value, it is taken as the pluck point as a
%      proportion of the string length (default 0.5).
%    v if present and nonzero plots waves & string at each sample time.
% 2001-02-01 dpwe@ee.columbia.edu.  Based on Julius Smith's 1992 CMJ paper.
% $Header: $

if nargin < 2
  d = 10000;
end
if nargin < 3 | length(r) == 0
  r = [-.25 -.5 -.25];
end
if nargin < 4 | length(x) == 0
  x = 0.5;
end
if nargin < 5
  viz = 0;
end

% where to read output from - residual at bridge in this case
pickup = len;

% Is r a filter or a mid-point?
if length(r) == 1
  % Single r value is mid point of 3 point filter - convert into actual filter
  r = -1/(r+2)*[1 r 1];
end

firlen = length(r);
firHlen = floor(firlen/2);

% Make right-going rail long enough to hold [r] points centered 
% at the end point (for calculating zero-phase reflection)
rlen = len + firlen - firHlen;
right = zeros(1,rlen);

left = zeros(1,len);

% Does x specify pluck point?
if length(x) == 1
  % Interpret as proportion along string at which it is plucked
  pluck = x*(len-1);
  % Create a triangular initial profile 'plucked' at this point
  x = [ [0:floor(pluck)]/pluck, ...
	(len - 1 - [(floor(pluck)+1):(len-1)])/(len - 1 - pluck) ];
end

% Initialization
if length(x) < len
  % If initial waveform is shorter than waveguide, pad with zeros
  % keeping specified part centered
  dl = len - length(x);
  x = [zeros(1,floor(dl/2)), x, zeros(1,ceil(dl/2))];
end

% Because initial velocity profile is flat, initial displacement 
% profile is equal in leftgoing and rightgoing waves.
left(1:len) = x(1:len)/2;
right(1:len) = x(1:len)/2;

% Initialize output
y = zeros(1,d);

% Initialize variables for display
pkval = max(abs(x));
ii = 0:(len-1);

% Execute waveguide
for t = 1:d
  
  if viz
    % Plot left and right-moving waves, and their sum
    plot(ii, left, ii, right(1+ii), ii, left+right(1+ii));
    % Make sure the axis scaling stays the same for each plot
    axis([0 len-1 -pkval pkval]);
    pause
  end
    
  % Move left-hand moving wave one step left; append dummy value for now
  left = [left(2:len),0];
  % At 'nut' (left-hand end), assume perfect reflection, so new value 
  % of right-moving wave is negative of new value at nut of left-moving
  nut = -left(1);
  % Move right-moving wave one step (including extra point off end)
  right = [nut, right(1:(rlen-1))];
  % Apply 'bridge' filter to end points of right-moving wave to get 
  % new value to fill in to end of left-moving wave
  % One point of convolution
  bridge = r * right( (len-firHlen-1)+[1:firlen] )';
  left(len) = bridge;  

  % Read output
  y(t) = left(pickup) + right(pickup);

end
