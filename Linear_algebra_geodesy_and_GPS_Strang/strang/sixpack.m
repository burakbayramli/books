more off
clc
echo on
% SIXPACK shows the six orderings for the loops in matrix multiplication.
% The first order is i then j then k, which finds dot products row by row.
% The second order will be kji.  You will see the difference as A*B is found.
% Here is the algorithm for the first multiplication order - then the result.
% 
% C = zeros(n,n);
% for i = 1:n
%    for j = 1:n
%       for k = 1:n
%          C(i,j) = C(i,j) + A(i,k)*B(k,j)
%          pause(1)
%       end
%    end
% end
%
%press any key
echo off
pause
clc
disp(' ')
disp(' ')
disp(' ')
disp(' ')
disp(' ')
disp(' ')
disp(' ')
disp(' ')

A = [2 3 1; -2 -3 -1; 1 1 1]
B = [1 4 3;  3  3  3; 1 2 2]
[n,n] = size(A);

C = zeros(n,n);
for i = 1:n
   for j = 1:n
      for k = 1:n
         home
         disp('The first order is i then j then k')
         C(i,j) = C(i,j) + A(i,k)*B(k,j)
         pause(1)
      end
   end
end

disp('The next order is k then j then i. Press any key ')

pause



C = zeros(n,n);
clc
disp(' ')
disp(' ')
disp(' ')
disp(' ')
disp(' ')
disp(' ')
disp(' ')
disp(' ')

A = [2 3 1; -2 -3 -1; 1 1 1]
B = [1 4 3;  3  3  3; 1 2 2]
for k = 1:n
   for j = 1:n
      for i = 1:n
         home
         disp('The second order is kji - which gives columns times rows. Why??')
         C(i,j) = C(i,j) + A(i,k)*B(k,j)
         pause(1)
      end
   end
end
disp('Edit this program to produce the other four orders of multiplication ')


