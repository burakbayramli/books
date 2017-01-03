function y = travel1(len,d,r,x,viz)
% y = travel1(l,d,r,x,v)  Visualize reflected travelling wave in waveguide
%    l is the length of each waveguide in samples.  
%    d is the number of output samples to produce (default 10000).
%    r is the refelection FIR at the far end (default [-1])
%    x is optional initial input waveform ([1] by default)
%    v if present and zero suppresses plotting.
% 2001-02-01 dpwe@ee.columbia.edu.  Based on pluck1a.m
% $Header: $

if nargin < 2
  d = 10000;
end
if nargin < 3
  r = [-1];
end
if nargin < 4 | length(x) == 0
  x = [1];
end
if nargin < 5
  viz = 1;
end

firlen = length(r);
firHlen = floor(firlen/2);

% Make right-going rail long enough to hold [r] points centered 
% at the end point (for calculating zero-phase reflection)
rlen = len + firlen - firHlen;
right = zeros(1,rlen);

left = zeros(1,len);

% Initialize output
y = zeros(1,d);

% Initialize variables for display
pk = max(abs(x));
ii = 0:(len-1);

% Execute waveguide
for t = 1:d
  
  if viz
    % Plot left and right-moving waves, and their sum
%    plot(ii, left, ':b', ii, right(1+ii), ':r', ii, left+right(1+ii), '-k');
    % Make sure the axis scaling stays the same for each plot
%    axis([0 len-1 -pkval pkval]); 
    subplot(311)
    plot(ii, right(1+ii), 'r'); 
    axis([0 len-1 -pk pk]); title('Right-moving wave f^+')
    subplot(312)
    plot(ii, left+right(1+ii), 'k'); 
    axis([0 len-1 -pk pk]); title('Resultant displacement');
    subplot(313)
    plot(ii, left, 'b'); 
    axis([0 len-1 -pk pk]); title('Left-moving wave f^-')
    pause
  end
    
  % Move left-hand moving wave one step left; append dummy value for now
  left = [left(2:len),0];
  % At 'nut' (left-hand end), assume perfect reflection, so new value 
  % of right-moving wave is negative of new value at nut of left-moving
  nut = -left(1);
  % Add input if we have any
  if t <= length(x)
    nut = nut + x(t);
  end
  % Move right-moving wave one step (including extra point off end)
  right = [nut, right(1:rlen-1)];
  % Apply 'bridge' filter to end points of right-moving wave to get 
  % new value to fill in to end of left-moving wave
  % One point of convolution
  bridge = r * right( (len-firHlen-1)+[1:firlen] )';
  left(len) = bridge;  

  % Read output
  y(t) = left(len) + right(len);

end
