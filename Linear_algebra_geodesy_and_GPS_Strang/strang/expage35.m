%EXPAGE35 Tutorial of Markov programming exercise.  
%
%       This program is a tutorial of MATLAB using
%       the first programming example from Gilbert Strang's
%       ``Introduction to Linear Algebra.''
%       This is Exercise 30 starting on page 35.
%
%       Written by T. Bryan on 16 August 1993. 
%        
clc
echo on

% This program is a tutorial of MATLAB using the
% first programming example from Gilbert Strang's
% ``Introduction to Linear Algebra,'' page 35-36, exercise 30.

% It will be helpful if you have your book open to the exercise.


% This tutorial can be terminated at any time by
% holding down on the ``Control'' key and pressing
% ``c'' on those terminals that have a control key.

pause % Strike any key to continue
clc

% This exercise explores some properties of a
% Markov matrix, named for the Russian
% mathematician A. A. Markov (1856--1922).  A
% Markov matrix has no negative entries and the sum
% down each column is 1.  Markov matrices appear in
% probability theory. To tell you their
% properties would give away the answers to
% Professor Strang's exercises.  Experiment with
% MATLAB to find their properties.

pause % Strike any key to continue
clc

% The components in the vectors that you are given
% in exercises 29--32 all sum to 1.  They are "probability vectors".


% Let's begin with the programming example in
% exercise 30, p. 36 and explain some of the basic
% MATLAB syntax as we go.  The code will plot a picture
% PROVIDED YOU OPEN A WINDOW. Note: The plot in Exercise 32 is much better!
% For Exercise 32 see the file expage36. The commands in Exercise 30 are

% u = [ 1 ; 0 ]; A = [.8 .3 ; .2 .7];
% x  = u;  k = [ 0:1:7 ];
% while length(x) <= 7
%   u = A*u;  x = [x u];
% end
% plot(k,x)

pause % Strike any key to continue
clc

% Let's examine this script one line at a time.

% This is a MATLAB script file, often called a .m file.
% All computer systems except the Macintosh use an extension
% after the dot; here it is m. The file contains a sequence 
% of MATLAB commands that you want to execute repeatedly.  

pause % Strike any key to continue
clc

% Every line of this script can be executed from
% the MATLAB command line (even loops) so if you
% wish to type the commands at the command line
% without storing them in a file, the results will
% be the same (except that you will have to
% re-type the entire sequence each time you wish
% to execute it).

pause % Strike any key to continue
clc

% Notice that all of these comments begin with the
% percent character (%).  In MATLAB, anything
% typed after a percent sign is a comment and not a command.

% This file begins with the command ``echo on'' which
% displays every command as it is executed, even
% the comments.  Usually we run with ``echo off''
% when we want the results without watching every step.

pause % Strike any key to continue
clc

% The first line of exercise 30 defines a vector u
% and a matrix A.

u = [1; 0]; A = [.8 .3; .2 .7];


% The command

u = [1; 0];

% creates a matrix that has two rows and one
% column (matrices with only one row or column are
% commonly called vectors).  This definition of a
% matrix starts with a left square bracket ``[''
% and ends with a right square bracket ``]''.  The
% semicolon between numbers indicates the start of
% a new row.  New rows may also begin on new lines.

pause % Strike any key to continue
clc

% Thus, the declaration 

u = [1; 0];

% is equivalent to 

u = [ 1
      0 ];


% The end of a command can be indicated in one of
% three ways:
%   (1) by a semicolon ;
%   (2) by a comma ,
%   (3) by a return
% Ending a command with a semicolon stops the
% display of the results of the command, while
% ending with a comma or carriage return do not.

pause % Strike any key to continue
clc

% A handy way to see the value of any variable is
% to type the variable on a line by itself with no
% semicolon, like so

u

pause % Strike any key to continue
clc

% Ending a command with a semicolon or comma
% allows us to put more than one command on a line.


u = [1; 0]; A = [.8 .3; .2 .7];


% The command

A = [.8 .3; .2 .7];

% creates a matrix with two rows and two columns.
% The rows are separated by the semicolon
% between .3 and .2.  An other way to enter A is

 A = [.8 .3
      .2 .7];

pause % Strike any key to continue
clc

% The columns of a matrix can either be separated
% by a space or a comma.  Thus, another equivalent
% method of defining matrix A is

A = [.8, .3
     .2, .7];


% It is more clear to separate columns of a matrix
% with commas when the elements contain
% expressions such as

[4, -2; 4-2, +2 ]

pause % Strike any key to continue
clc

% Typing A by itself displays its contents

A

pause % Strike any key to continue
clc

% The next command

x = u;

% creates a variable called x and assigns it the
% same value as u.  

% Thus

x
u

pause % Strike any key to continue
clc

% The next command

k = [ 0:1:7 ];

% is perhaps one of the most useful features of
% MATLAB: the creation of regularly spaced vectors
% using the ``colon'' notation.  The command means
% ``create a variable called k and fill it with a
% row vector that goes from zero to 7 in steps of 1.


% Thus, the elements of k are

k

pause % Strike any key to continue
clc

% The colon notation can be used to create vectors
% stepping in the opposite direction

9: -1 : 4

pause % Strike any key to continue
clc

% and they can start and stop and be stepped by
% any real number, not just integers

-3*pi: pi: 3*pi

pause % Strike any key to continue
clc

% The next command

% while length(x) <= 7

% is the start of a loop.  The following commands
% until the ``end'' command will be executed
% as long as the statement following ``while'' is true.

pause % Strike any key to continue
clc

% The command ``length(x)'' returns the greater of
% the row and the column dimension.  At the start
% of the loop, x is a 2-by-1 vector, thus

length(x)

% should have returned 2.  

pause % Strike any key to continue
clc

% As we will see, each time we execute the body of
% the loop, one more column is added to x, but the
% number of rows stays the same.  Thus, we are
% asking our loop to continue while the number of
% columns of x is less than or equal to seven.

pause % Strike any key to continue
clc

% The first command inside the loop asks us to
% multiply vector u by matrix A, and replace the
% contents of u by the product.  Thus, the old u is
    
u


% and the new u is

u = A*u

pause % Strike any key to continue
clc

% The next command appends the vector u to the
% vector x and replaces x with the new matrix.
% Thus, the old x is

x


% and the new x is
 
x=[x u]

% This demonstrates a convenient feature of MATLAB
% for creating new matrices.

pause % Strike any key to continue
clc

% The ``end'' command indicates the end of the
% body of the loop.  Now let's allow the loop to
% continue, displaying the intermediate results:
   
 
u = [1;0] ; A = [.8 .3; .2 .7];
x = u; k = [0:1:7];
while length(x) <= 7
  u = A*u
  x = [x u]

disp('Strike any key to continue'); pause;

end


% The last command

plot(k,x)

% plots these eight vectors. Above the numbers 0 to 7 are the 
% two components of the vector at that step. REMEMBER:
% Exercise 32 which is described in expage36 gives a much better plot.

pause % Strike any key to continue
clc

% To complete the exercises in the book, replace

u = [1; 0];

% with

u = [0; 1];

% and 

u = [.5; .5];

% in this MATLAB script and observe the results.
% The plots should all show the same limit [.6 .4].
% This is the "steady state" at the right hand end of the plot.
 
% To save the results of the plot on paper, the
% command is ``print'' (or ``prtsc'' on some
% machines) from the MATLAB command line.

% This concludes the tutorial for exercise 30 on
% Markov matrices.

echo off


