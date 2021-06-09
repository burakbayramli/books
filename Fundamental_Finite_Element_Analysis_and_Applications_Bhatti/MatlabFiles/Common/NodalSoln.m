function [d, rf] = NodalSoln(K, R, debc, ebcVals)
% [nd, rf] = NodalSoln(K, R, debc, ebcVals)
% Computes nodal solution and reactions
% K = global coefficient matrix
% R = global right hand side vector
% debc = list of degrees of freedom with specified values
% ebcVals = specified values
dof = length(R);
df = setdiff(1:dof, debc);
Kf = K(df, df);
Rf = R(df) - K(df, debc)*ebcVals;
dfVals = Kf\Rf;
d = zeros(dof,1);
d(debc) = ebcVals;
d(df) = dfVals;
rf = K(debc,:)*d - R(debc);
