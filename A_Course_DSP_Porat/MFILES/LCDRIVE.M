% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

disp('Make sure qtype, when, rtype, a1, a2, B, M are defined');
r = roots([1,a1,a2]);
if (max(abs(r)) >= 1),
   disp('Input filter is unstable'); return, end
if (imag(r(1)) == 0),
   disp('Poles are real'); return, end
if (rtype == 'c'), apar = [real(r(1)),imag(r(1))];
else, apar = [a1,a2]; end
n = ceil(-2*B*log(2)/log(abs(r(1))));
for i = 1:M,
   s0 = rand(1,2)-0.5*ones(1,2);
   flag = lc2sim(qtype,when,rtype,apar,B,s0,n);
   if (flag == 1),
      disp('DC limit cycle exists!'); return
   elseif (flag == 2),
      disp('Non-DC limit cycle exists!'); return
   end
end
disp('Apparently limit cycle free!');

