function b = betalog(r,s);
% log of a beta distribution
b = gammaln(r)+gammaln(s) - gammaln(r+s);