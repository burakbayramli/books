function L = lpca2lsp(A)
% L = lpca2lsp(A)   Convert LPC filter coefficients to Line Spectral Pairs
%       Each row of A is a full LPC filter definition (column 1 is forced 
%       to be 1.0).  Return corresponding rows of line spectral pairs.
% 2001-03-20 dpwe@ee.columbia.edu

[nrows, ncols] = size(A);

order = ncols - 1;

% Rewrite the first column (in case it's got gains in it)
A(:,1) = ones(nrows,1);

L = zeros(nrows, order);

for row = 1:nrows

  aa = [A(row,:),0];  % have to pad by one 
  aar = fliplr(aa);
  P = aa + aar;
  Q = aa - aar;
  Proots = sort(angle(roots(P)))/(2*pi);
  Qroots = sort(angle(roots(Q)))/(2*pi);

  % Interleave roots
  L(row,1:2:order) = Proots(order/2 + [1:(order/2)])';
  L(row,2:2:order) = Qroots(order/2 + 1 + [1:(order/2)])';

end