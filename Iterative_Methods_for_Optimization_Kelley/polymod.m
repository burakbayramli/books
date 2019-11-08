function [lplus]=polymod(q0, qp0, lamc, qc, blow, bhigh, lamm, qm)
%
% C. T. Kelley, Dec 29, 1997
%
% This code comes with no guarantee or warranty of any kind.
%
% function [lambda]=polymod(q0, qp0, qc, blow, bhigh, qm)
%
% Cubic/quadratic polynomial linesearch
%
% Finds minimizer lambda of the cubic polynomial q on the interval
% [blow * lamc, bhigh * lamc] such that
%
% q(0) = q0, q'(0) = qp0, q(lamc) = qc, q(lamm) = qm
% 
% if data for a cubic is not available (first stepsize reduction) then
% q is the quadratic such that
% 
% q(0) = q0, q'(0) = qp0, q(lamc) = qc
%
lleft=lamc*blow; lright=lamc*bhigh; 
if nargin == 6
%
% quadratic model (temp hedge in case lamc is not 1)
% Thanks to the Tulane grad students for fixing an error
% in Spring 2016!
%
%    lplus = - qp0/(2 * lamc*(qc - q0 - qp0) );
    lplus = - (qp0*lamc*lamc)/(2 * (qc - q0 - qp0*lamc) );
    if lplus < lleft lplus = lleft; end
    if lplus > lright lplus = lright; end
else
%
% cubic model
%
    a=[lamc^2, lamc^3; lamm^2, lamm^3];
    b=[qc; qm]-[q0 + qp0*lamc; q0 + qp0*lamm];
    c=a\b;
    lplus=(-c(1)+sqrt(c(1)*c(1) - 3 *c(2) *qp0))/(3*c(2));
    if lplus < lleft lplus = lleft; end
    if lplus > lright lplus = lright; end
end
