function [fout,ifail,icount]=lc_obj(x)
% LC_OBJ
%
% Hardwire the linear constraint x_1 + x_2 >= 1 into
% the objective to apply the extreme barrier approach
% to constraints.
%
if x(1)+x(2) < 1
   fout=NaN;
   ifail=1;
   icount=0;
else
   fout1=(x(1)-.5)^2; 
   fout2=.25*(1-x(1))^2*(1 - x(2))^2;
   fout3= .1*(x(1)-.5)^2*(1 + x(2) - 2* x(2)^2); 
   fout=fout1+fout2+fout3;
   ifail=0;
   icount=1;
end

