function A = lpclsp2a(L)
% A = lpclsp2a(L) Convert Line Spectral Pairs to LPC filter coefficients
%      Reverse of lpca2lsp
% 2001-03-20 dpwe@ee.columbia.edu

[nrows,order] = size(L);

A = zeros(nrows,order+1);

for row = 1:nrows

   Proots = exp(j*2*pi*L(row,1:2:order));
   Qroots = exp(j*2*pi*L(row,2:2:order));

   Proots = [Proots,conj(Proots),-1];
   Qroots = [Qroots,conj(Qroots),1];

   P = poly(Proots);
   Q = poly(Qroots);

   A(row,:) = .5*(P(1:(order+1))+Q(1:(order+1)));

end