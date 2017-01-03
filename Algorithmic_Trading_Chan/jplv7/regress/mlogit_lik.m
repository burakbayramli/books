function [g,g1,p,dev]=lik(y,x,beta,z,z1)
% PURPOSE: Calculates likelihood for multinomial logit regression model.

% written by:
%  Gordon K Smyth, U of Queensland, Australia, gks@maths.uq.oz.au
% Nov 19, 1990.  Last revision Aug 29, 1995.

e=exp( [z x]*beta ); e1=exp( [z1 x]*beta );
g=e./(1+e); g1=e1./(1+e1);
g=max( y==max(y),g ); g1=min( y>min(y),g1 );
p=g-g1;
dev=-2*sum(log(p));