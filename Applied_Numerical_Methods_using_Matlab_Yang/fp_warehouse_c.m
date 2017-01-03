function [C,Ceq]=fp_warehouse_c(x)
C=sum(abs(x-[1 1]))-2; 
Ceq=[]; % No equality constraint 
