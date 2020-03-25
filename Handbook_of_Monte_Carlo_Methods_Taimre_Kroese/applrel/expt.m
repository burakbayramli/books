function x=expt(u,a,b)
% generate from truncated exponential on [a,b]
% with coefficient 'u'

const=exp(-u*a)-exp(-u*b);

x=-log(exp(-u*a)-const*rand)/u;
