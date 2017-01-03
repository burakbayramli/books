function y = ppsrc(x,h,L,M);
% Synopsis:  y = ppsrc(x,h,L,M).
% Sampling-rate conversion by polyphase filters.
% Input parameters:
% x: the input sequence
% h: the conversion filter
% L, M: the interpolation and decimation factors.
% Output parameters:
% y: the output sequence

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

ML = M*L; lh = length(h); lx = length(x);
ly = floor((L*lx+lh-2)/M)+1; % length of the result
K = floor((lh-1)/ML)+1; % max length of polyphase components
r = zeros(ML,K); % storage for polyphase components
for l = 0:L-1, % build polyphase components
for m = 0:M-1,
   temp = h(rem(l*M+(M-m)*L,ML)+1:ML:lh);
   if (length(temp) > 0),
   r(M*l+m+1,1:length(temp)) = temp; end
end, end
x = [reshape(x,1,lx),zeros(1,M)]; % needed for the 1 delay
lx = lx + M;
lu = floor((lx-1)/M) + 1; % length of the sequences u_m
x = [x, zeros(1,M*lu-lx)];
x = reshape(x,M,lu); % now the rows of x are the u_m
y = zeros(L,K+lu-1);
for l = 0:L-1, % loop on sequences v_l
for m = 0:M-1, % loop on sequences u_m
   if (m <= floor(l*M/L)),
      temp = x(m+1,:);
   else
      temp = [0,x(m+1,1:lu-1)];
   end
   y(l+1,:) = y(l+1,:) + conv(r(M*l+m+1,:),temp);
end, end
y = reshape(y,1,L*(K+lu-1));
y = y(1:ly);

