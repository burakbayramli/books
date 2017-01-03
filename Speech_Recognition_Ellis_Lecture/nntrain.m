function [woh,woo,es] = nntrain(i,N,o,lr,its,wih,wio,bu,v)
% [woh,woo,es] = nntrain(i,N,o,lr,its,wih,wio,b,v) Train n net with 1 hid lyr
%    i is a matrix of input pattern rows; 
%    N is normalization [means; stds] for data in i (empty: calc from i)
%    o are corresponding output pattern rows.
%    wih is the initial set of input-to-hidden weights
%      if wih is a scalar, it is the initialized hidden layer size.
%    wio is the initial set of hidden-to-output weights
%    lr is the learning rate (default 0.1)
%    its is the number of iterations to perform (default 10)
%    b is the bunch size - patterns calculated before weight update (dflt 16)
%    v is verbosity - report MSE once every v iterations
%    Perform online backprop of mean-square error for sigmoid units.
%    weight matrices have one extra row for biases.
%    es returns the mse/pat for each iteration
% 2001-02-08 dpwe@ee.columbia.edu
% $Header: $

isize = size(i, 2);
osize = size(o, 2);

npat = size(i, 1);

% Random presentation order
ord = randperm(npat);

if nargin < 4
  lr = 0.01;
end
if nargin < 5
  its = 10;
end
if nargin < 6
  wih = 10;
end
if length(wih) == 1
  wih = rand(wih, isize + 1);
end
nhid = size(wih,1);
if nargin < 7
  wio = [];
end
if isempty(wio)
  wio = rand(osize, nhid + 1);
end
if nargin < 8
  bu = 0;
end
if nargin < 9
  v = 1;
end
if bu == 0
  bu = 16;
end

if length(N) == 0
%  N = ones(2, isize);
   N = [mean(i);std(i)];
end

woh = wih;
woo = wio;

es = zeros(1,its);

% How many patterns to train at once (before updating weights)
% (faster because can use matrix ops to accelerate fwd pass)

bunchsize = bu;

for it = 1:its

  sumE2 = 0;

  for n = 1:bunchsize:npat
    ipx = n:min(n+bunchsize-1,npat);
    nbun = length(ipx);
    ip = (i(ord(ipx),:) - ones(nbun,1)*N(1,:))./(ones(nbun,1)*N(2,:));
    op = o(ord(ipx),:);

    % Calculate forward 
    hidsum = woh*[ip,ones(nbun,1)]';
    hidact = 1./(1+exp(-hidsum));
    outsum = woo*[hidact;ones(1,nbun)];
    outact = 1./(1+exp(-outsum));

    % Calculate error
    E2 = sum((outact - op').^2);
    sumE2 = sumE2 + sum(E2);

    % Backpropogate
    Do = outact - op';
    Dh = Do' * woo(:,[1:nhid]);

    dEdxo = 2 * Do .* (outact .* (1 - outact));
    dEdwo = dEdxo * [hidact;ones(1,nbun)]';
    woo = woo - lr*dEdwo;

    dEdxh = 2 * Dh' .* (hidact .* (1 - hidact));
    dEdwh = dEdxh * [ip';ones(1,nbun)]';

    woh = woh - lr*dEdwh;
  
  end  % loop over patterns
  
  es(it) = sqrt(sumE2/npat);
  if v > 0 & rem(it,v) == 0
    disp(['Iteration=', num2str(it), ' MSError =', num2str(es(it))]);
  end
  

end  % iteration loop

