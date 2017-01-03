echo on
clc
% "House" data set:  The 2 by 12 matrix for the coordinates of the
% house in the book.  A "Better House" is in xplot2d with more
% discussion and examples. 
%
%  X = [ -6  -6  -7   0   7   6   6  -3  -3   0   0  -6
%        -7   2   1   8   1   2  -7  -7  -2  -2  -7  -7 ];

X = [ -6  -6  -7   0   7   6   6  -3  -3   0   0  -6
   -7   2   1   8   1   2  -7  -7  -2  -2  -7  -7 ];

%  plot2d(X) displays the following figure from Section 7.1.
%  The first row of X is the horizontal axis, the second row
%  is the vertical axis.  Both axes run from -10 to 10.
%
%
%                   o                    
%                 /   \                  
%               /       \                
%             /           \              
%           /               \            
%         /                   \          
%       o                       o        
%     o |                       | o      
%       |                       |        
%       |                       |        
%       |     o - - o           |        
%       |     |     |           |        
%       |     |     |           |        
%       |     |     |           |        
%       |     |     |           |        
%       o - - o - - o - - - - - o        
%
%press any key
pause
clc
%  What would you expect to see with
%
%      plot2d(A*X)
%
%  for each of the following matrices A?
%  Try to predict the result and then draw the picture.
%  
%  [1 0; 0 -1]
%  [-1 0; 0 1]
%  [0 1; -1 0
%  [0 1; 1 0]
%  [.5 0; 0 .5]
%  [2 0; 0 2]
%  [3 0; 0 3]
%  [0 1; 0 0]
%  [0 0; 0 -2]
%  [1 0; 1/3 2/3]
%  [1 1/3; 0 2/3]
%  [-1 1/3; 0 2/3]
%  [1 1/4; 1/4 1]
%
%  Let
A = [.8 .6; -.6 .8]
%  What do you expect from:
%
%  plot2d(A*X)
%  plot2d(A*A*X)
%  plot2d(inv(A)*X)
%  plot2d(A'*X)
%  plot2d(abs(A)*X)
%
% The next codes show rotations of the house. OPEN A FIGURE WINDOW . 
% First come powers of A (press any key for each new house) .
%
for k = 0:5
   plot2d(A^k * X);
   pause
end
% Now come six random rotations:
for k = 1:6
   r = 8*(2*rand-1);
   A = [cos(r) sin(r); -sin(r) cos(r)];
   plot2d(A*X)
   pause
end
echo off
