function [fv,ifail,icount]=f_easy(x)
% F_EASY
% Simple example of using imfil.m
%
%function [fv,ifail,icount]=f_easy(x)
%
fv=x'*x;
fv=fv*(1 + .1*sin(10 * (x(1) + x(2)) ));
%
% This function never fails to return a value 
%
ifail=0;
%
% and every call to the function has the same cost.
%
icount=1;
