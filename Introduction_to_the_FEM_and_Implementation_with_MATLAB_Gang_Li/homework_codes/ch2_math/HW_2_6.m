clear all;
disp('The given matrix,')

A = [ 1  4                 % enter the given matrix
      2 -1]
X = 2*eye(2);                    
Y = X - A;
Z = det(Y);                % find the value of first determinant

disp('  ')
fprintf('The value of det(2*eye(2)-A) is: %.2f \n',Z)
disp('  ')

for t=1:100
  P = t*eye(2)-A;        % the matrix value of second determinant      
  if det(P) == 0          % check whether the 2nd determinant equals zero
    fprintf('The value of t such that det(tI-A)=0 is: %d \n',t)
  end
end