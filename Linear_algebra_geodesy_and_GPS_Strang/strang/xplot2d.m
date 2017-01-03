echo on
clf
clc
% The front cover of the book shows "houses" that were drawn using plot2d.
% Section 7.1 describes how they illustrate linear transformations.
% The corner points of the original house are in the house matrix H. 
%
% A*H gives the transformed corner points, with coordinates multiplied by
% the 2 by 2 matrix A.
% We now give the matrix BH for the "corners" of a BETTER HOUSE.
% You can always use the matrix H for the houses in the book.

BH = [-3. -3. -5. -5. -6. -4. 2. 4. -2. 3.      3. -3. -3. -4. -2.;
       2. -6. -4.  4.  3.  5. 7. 1. -1. 0.6667 -4. -6.  2.  5. -1.];

% This house has more of a 3D appearance
plot2d(BH)
pause(1)
% press any key
pause
% Transform the house by any 2 by 2 matrix A. If the matrix A is too large 
% the better house will not fit in the square. A first example is

A = [0 1; 1 0];
plot2d(A*BH)
pause(1)

% Can you guess which matrix A transforms to this third house?
% press any key to see the third house
pause
echo off
A = [0 -1; 1 0];
plot2d(A*BH)
pause(3)
clc
echo on
%  
%  Press any key to play with other transformations
%  
%  We have now added a square called SQ - a very minimal house.
pause
clf
echo off
X = BH;
i = 'Y';
a = strcmp(i,'N') ;
while a == 0
   i = input('Which object do you want? Type one of H,BH,SQ (NO SPACE!) then return: ','s');
   if i == 'H'
      X = [-6 -6 -7 0 7 6  6 -3 -3  0  0 -6;
           -7  2  1 8 1 2 -7 -7 -2 -2 -7 -7];
   end
   if i == 'BH'
      X = [-3. -3. -5. -5. -6. -4. 2. 4. -2. 3.      3. -3. -3. -4. -2.;
            2. -6. -4.  4.  3.  5. 7. 1. -1. 0.6667 -4. -6.  2.  5. -1.];
   end
   if i == 'SQ'
      X = [0 0 5 5 0
           0 5 5 0 0];
   end
   A = [1 0;0 1];
   C = A;
   b = input('Input A in the form [a b; c d] without A = and then return ');
   if isempty(b);
      b = C;
   end
   A = b;
   C = A*X;
   p = C(1,:)';
   q = C(2,:)';
   plot(p,q,'-')
   text(-4.5,-11.2,'A=')
   text(-3,-10.8,[num2str(A(1,1)),'  ',num2str(A(1,2))])
   text(-3,-11.8,[num2str(A(2,1)),'  ',num2str(A(2,2))])
   axis([-10 10 -10 10])
   axis('square')
   pause(3)
   %axis off
   i=input('Do you want to draw another one? Y or N: ','s');
   a = strcmp(i,'N');
end

