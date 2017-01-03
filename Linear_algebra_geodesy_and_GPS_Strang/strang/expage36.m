%MARKOV2 Tutorial of Markov programming exercise.  
%
%       This program is a tutorial of MATLAB using
%       the second programming example from
%       "Introduction to Linear Algebra" page 36 exercise 32.

%       Written by T. Bryan on 16 August 1993. 
%        
clc
echo on

%
%   This program is a tutorial of MATLAB using
%   the second programming example from
%   "Introduction to Linear Algebra" page 36 exercise 32.

%   It will be helpful if you have your book open to page 36.

%   This tutorial can be terminated at any time by
%   holding down on the ``Control'' key and pressing
%   ``c'' on those terminals that have a control key.

pause % Strike any key to continue
clc

% By construction, the initial probability vector
% that you will be choosing by clicking your mouse
% button on the plot is assured to have elements
% between 0 and 1, with sum a + b + (1 - a - b) = 1.


% As in exercise 30, start by defining a Markov matrix

A = [.8 .2 .1; .1 .7 .3; .1 .1 .6]

pause % Strike any key to continue
clc

% The next few lines control the looks of the plot.
% OPEN A WINDOW.

clg
axis([0 1 0 1]); axis('square')
plot(0,0,0,1,1,0,1,1); hold on
axis([0 1 0 1]);axis('square')
title('Markov'); xlabel('a'); ylabel('b'); grid

pause % Strike any key to continue
clc

% As in exercise 30, we have a ``while'' loop,
% only this time we have two that are nested. 
% The "inner loop"

%  x = u; k = [0:1:7];
%  while length(x) <= 7
%    u = A*u; x = [x u];
%  end

% is exactly as in exercise 30.

pause % Strike any key to continue
clc

% The ``outer loop'' controls the input of data
% by clicking on the plot and checking to see if
% the left mouse button was pressed.
% but = 1;
% while but == 1
%   [a,b,but] = ginput(1)
%   u = [a; b; 1-a-b];
%   x = u; k = [0:1:7];
%   while length(x) <= 7
%     u = A*u; x = [x u];
%   end
%   plot(x(1,:),x(2,:),x(1,:),x(2,:),'o');
% end
pause % Strike any key to continue
clc

% Let's examine some new features here.  The command

but = 1;

% initializes a variable named ``but'' (short for
% ``button'') that indicates which mouse or keyboard button was
% pressed.  The value ``1'' indicates that the
% left mouse button (or only mouse button) was pressed.

pause % Strike any key to continue
clc

% The value of the variable ``but'' is set in the
% function call

%   [a,b,but] = ginput(1)

% This line brings up another important
% programming feature: that of a function call.
% We pass information into a function via its
% input parameters, and we get information back
% from its output parameters.  In MATLAB we pass
% information to a function surrounded by round
% brackets following the function name.  

pause % Strike any key to continue
clc

% In this case, we tell the function ``ginput''
% (for graphical input) to get one data point from
% the graph when we click the mouse button over
% the graph by saying ``ginput(1)''.    If we had
% wanted two data points from two clicks, we would
% say ``ginput(2)''.  The data returned from our
% key click comes in (x,y) pairs, plus a code for
% the keys pressed in the third output variable.
% The mouse buttons return 1,2,... from left to right.

pause % Strike any key to continue
clc

% Thus,

%     [a,b,but] = ginput(1)

% returns the pair (a,b) corresponding to the
% coordinates on the graph where we clicked, plus
% a code for the button or key that we pressed.
% Note that output parameters are surrounded
% by square brackets ``[a,b,but]'' while input
% parameters are surrounded by round ones ``(1)''. 

pause % Strike any key to continue
clc

% The command

%  u = [a; b; 1-a-b];

% ensures that the entries of  u are
% appropriate for a probability vector.
% ``a'' and ``b'' must be between 0 and 1
% because they are chosen from the unit square grid.
% The third coordinate ``1-a-b'' is not seen!!
% It ensures that the entries sum to 1 (a + b + 1-a-b = 1).

pause % Strike any key to continue
clc

% Now, let the program run.

echo off
clc
disp('Position the cursor over your ``a'''' and ``b''''.')
disp('Click the left button. The picture will show the')
disp('vector multiplied several times by A. ')
disp('Remember that you only see components 1 and 2.')
disp(' ')
disp('Do more clicks to make a Markov picture.')
disp('In our experience the start is not right under the cursor.')
disp(' ')
disp('To quit, position the cursor over the plot ')
disp('and press any key on the keyboard.')

but = 1;
while but==1
  [a,b,but]=ginput(1)
  u=[a; b; 1-a-b];
  x=u; k=[0:1:7];
  while length(x)<=7
    u=A*u; x=[x u];
  end
  plot(x(1,:),x(2,:),x(1,:),x(2,:),'o');
end
%hold off

echo on
% This concludes the tutorial for exercise 32 on
% Markov matrices.

echo off

