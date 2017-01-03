function P = xcorrpeak(X,Y)
% P = xcorrpeak(X, Y) Find cross-correlation peak skew
%     X and Y are equal-length vectors.  Compute their cross correlation 
%     and return the optimal delay for X to align it to Y.
% 2002-04-01 dpwe@ee.columbia.edu  E6820 example

% Make sure X and Y are column vectors
if (size(X,1)) == 1
  X = X';
end
if (size(Y,1)) == 1
  Y = Y';
end

w = hanning(length(X));

% Time domain version
rxy = xcorr(w.*Y,w.*X);
mxr = max(rxy);
p = find(rxy == mxr) - length(X);
% In case of multiple maxima, just take the first one
P = p(1);


