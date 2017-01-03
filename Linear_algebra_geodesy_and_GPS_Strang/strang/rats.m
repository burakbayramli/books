function rats(A)
%RATS	Print in "rational" form.
%	     RATS(A) prints A with fractions instead of decimals.
%       This is only a display! You cannot assign a name B = rats(A).
tol = 1.e-6;
A = A .* (abs(A)>tol);

disp(' ');
v = version;
if v(1) == '4' | v(1) == '5'
   % MATLAB 4 and MATLAB 5 have a rational output format, so use it.
   f = get(0,'format');
   format rat
   disp(A)
   format(f);
else
   % MATLAB 3.5 and Student MATLAB do not have format rat.
   disp(rat(A,'s'))
end
disp(' ');
