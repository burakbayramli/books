function demoCopy
% copyDemo  Demonstrate vectorized copy operations
%
% Synopsis:  demoCopy
%
% Input:     none
%
% Output:    Messages describing various copy operations are given
%            along with the results of those operations.  See text
%            in Appendix A for additional description.

fprintf('\n\nCreate A matrix as source of values\n');
disp('>> A = [ 1  2  3;  4  5  6;  7  8  9]');   A = [ 1  2  3;  4  5  6;  7  8  9]

fprintf('\n\nCreate B matrix with same number of rows and columns as A\n');
disp('>> B = ones(size(A))');   B = ones(size(A))
pause

fprintf('\n\nCopy first column of A into B\n');
disp('>> B(:,1) = A(:,1)');   B(:,1) = A(:,1)
pause

BB = ones(size(B));
for i=1:3
   BB(i,1) = A(i,1);
end
fprintf('Error in loop version = %f\n',max(max(B-BB)));    %  vectorized error check

fprintf('\n\nReset B\n');  pause
B = ones(size(B))   %  Reset B
pause
fprintf('\n\nCopy part of the last column of A into first row B\n');  pause
disp('>> B(1,2:3) = A(2:3,3)''');   B(1,2:3) = A(2:3,3)'
pause

BB = ones(size(B));
for j=2:3
   BB(1,j) = A(j,3);
end
fprintf('Error in loop version = %f\n',max(max(B-BB)));    %  vectorized error check


fprintf('\n\nReset B\n');  pause
B = ones(size(B))
pause
fprintf('\n\nCreate a vector of four values\n');  pause
disp('>> x = 21:24');   x = 21:24
pause

fprintf('\n\nCopy elements of x into lower left corner of B\n');  pause
disp('>> B(2:3,1:2) = reshape(x,2,2)''');   B(2:3,1:2) = reshape(x,2,2)'

BB = ones(size(B));
k = 0;
for i=2:3
  for j=1:2
     k = k + 1;
     BB(i,j) = x(k);
  end
end

fprintf('Error in loop version = %f\n',max(max(B-BB)));    %  vectorized error check

