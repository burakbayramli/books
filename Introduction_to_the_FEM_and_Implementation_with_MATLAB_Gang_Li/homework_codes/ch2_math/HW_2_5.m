clear all;

disp('The 2x2 matrix,')

A11= [ 1  0           % enter the first matrix
       0 -1]
   
disp('  ')
disp('The 2x2 matrix,')

A12= zeros(2,2)       % enter the second matrix

disp('  ')
disp('The 2x2 matrix,')

A22= [ 0 1            % enter the third matrix
       1 0]

disp('  ')
disp('The 4x4 matrix,')

A = [A11 A12          % construct a 4x4 matrix
     A12 A22]

 
for n = 2:100          % Search for n greater than 1
    
  if A^n==A           % check if A^n matrix equals A matrix
     
     disp('  ')
     disp('The 4x4 A^n matrix,')

     Apowern=A^n       % display A^n matrix
     
     disp('  ')
     fprintf('The smallest value of n greater than 1 such that A^n=A is: %d \n',n)
     break
  end
end