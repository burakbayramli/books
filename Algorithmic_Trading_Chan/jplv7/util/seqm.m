function seq=seqm(a,b,c);
% PURPOSE: produce a sequence of values
% -----------------------------------------------------
% USAGE: y = seqm(a,b,c)
%  where    a = initial value in sequence 
%           b = increment
%           c = number of values in the sequence  
% -----------------------------------------------------
% RETURNS: a sequence, (a a*b ...(a*b^(c-1)))' in MATLAB notation
% ----------------------------------------------------- 
% NOTE: a Gauss compatability function
% -----------------------------------------------------

% written by:
% Peter M. Summers
% Melbourne Institute of Applied Economic & Social Research
% The University of Melbourne
% Parkville, Victoria 3010
% Australia

% seqm Gauss eqivalent of seqm(a,b,c)
seq = zeros(c,1);
seq(1) = a; 
if c>1;
   seq(2) = a*b;

for i = 3:c;
   seq(i) = seq(i-1)*b;
end;

end;

return;
