function p = poly2str(c,x)
%POLY2STR Convert a polynomial coefficient vector to a string.
%	P = POLY2STR(C) generates a string representation of the polynomial
%	whose coefficents are in the vector C.  The free variable is 'x',
%	unless otherwise specified by P = POLY2STR(C,'s').
%	The coefficients are approximated, if necessary, by the rational
%	values obtained from RATS.
%	
%	If X has a numeric value and the elements of C are reproduced
%	exactly by RATS, EVAL(POLY2STR(C)) will return the same value as
%	POLYVAL(C,X).
%
%	See also POLYVAL, RATS

if nargin < 2, x = 'x'; end
if all(c == 0), p = '0'; return, end

p = [];
n = length(c);
for d = 0: n-1
   if d > 0
      if c(n-d+1) > 0
         p = [' + ' p];
      elseif c(n-d+1) < 0
         p = [' - ' p];
      end
   end
   if c(n-d) ~= 0
      if d == 1
         p = [x p];
      elseif d > 1
         p = [x '^' int2str(d) p];
      end
      if (abs(c(n-d)) ~= 1) | (d==0)
         if d > 0,
            p = ['*' p];
         end
         [sn,sd] = rat(abs(c(n-d)));
         s = num2str(sn);
         if sd ~= 1, s = [s '/' num2str(sd)]; end
         p = [s p];
      end
   end
end
if n > 0
   if c(1) < 0
      p = ['-' p];
   end
end
