function  pd=poly_der(p)
%p: the vector of polynomial coefficients in descending order
N= length(p);
if N<=1,  pd= 0; % constant
else   
   for i=1: N-1, pd(i) =p(i)*(N-i); end
end 
