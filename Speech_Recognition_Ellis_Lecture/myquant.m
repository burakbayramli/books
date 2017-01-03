function [Q,I] = myquant(X,M,N)
% [Q,I] = myquant(X,M,N)  Quantize values uniformly
%    Q returns X quantized to N levels (default 256), and I returns 
%    integer indexes corresponding to each value in Q.  M specifies 
%    the range of levels; for M scalar (default 1.0), the values 
%    are between -M and M.  If M has two values, quantization is
%    between M(1) and M(2).  Q is 'dequantized' back to be an 
%    approximation of X; to get the quantized level, multiply 
%    N and divide by 2M (or M(2) - M(1)).
% 2001-03-20 dpwe@ee.columbia.edu

if nargin < 2
  M = 1.0;
end
if nargin < 3
  N = 256;
end

if length(M) == 1
  M = [-M,M];
end

qstep = (M(2) - M(1))/N;

% Q = max(M(1), min(M(2), M(1) + qstep*round( (X - M(1)) / qstep )));
I = max(0, min(N-1, round( (X - M(1)) / qstep )));
Q = M(1) + qstep*I;


