function [maxDDN,DDNmat] = ddnewton(x,v)
% Purpose: Create the table of Newtons divided differences based on (x,v)
m = length(x); DDNMat = zeros(m,m+1);
 
% Inserting x into the 1st column and f into 2nd colume of table
DDNmat(1:m,1) = x; DDNmat(1:m,2) = v;
 
% create divided difference coefficients by recurrence
for j = 1:m-1
   for k = 1:m-j
     DDNmat(k,j+2) = ...
         (DDNmat(k+1,j+1)-DDNmat(k,j+1))/(DDNmat(k+j,1)-DDNmat(k,1));
   end
end

% extract max coefficient
maxDDN = abs(DDNmat(1,m+1)); 
return