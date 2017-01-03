function o = nnfwd(i,N,wh,wo)
% o = nnfwd(i,N,wh,wo)  MLP3 forward pass
%    i is a matrix of input pattern rows; 
%    N is normalization [means; stds] for data in i
%    o are corresponding output pattern rows.
%    wh is the set of input-to-hidden weights
%    wo is the set of hidden-to-output weights
% 2001-02-08 dpwe@ee.columbia.edu
% $Header: $

isize = size(i, 2);
osize = size(wo, 1);
npat = size(i, 1);

if length(N) == 0
  N = ones(2, isize);
end

% Do all patterns at once
ip = (i - ones(npat,1)*N(1,:))./(ones(npat,1)*N(2,:));

% Calculate forward 
hidsum = wh*[ip,ones(npat,1)]';
hidact = 1./(1+exp(-hidsum));
outsum = wo*[hidact;ones(1,npat)];
outact = 1./(1+exp(-outsum));

o = outact';

  