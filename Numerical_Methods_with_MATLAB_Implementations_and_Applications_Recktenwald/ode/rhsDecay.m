function dydt = rhsDecay(t,y,flag,alpha)
% rhsDecay  Evaluate rhs of dy/dt = -alpha*y with a variable alpha.
%           "flag" parameter is required for compatability with ode45
dydt = -alpha*y;
