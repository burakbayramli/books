clc
echo on
% This program creates matrices with 0's and 1's at random.  It counts the
% number of experiments that give a singular matrix.  It divides that number
% by the total number of matrices to give the fraction that are singular.
% I don't know what the limit of that fraction is, but
% THE CODE WILL RUN FOREVER.    Hit <ctrl-C> to stop it.
%
% You could edit the code to do 2 by 2 and 4 by 4. Also -1's and 0's and 1's.
% Press any key to start the 3 by 3 experiment. 
pause

echo off
total  = 0;
singular = 0;
clc
while 1
   home
   A = round(rand(3,3))
   if det(A) == 0, singular = singular + 1; end
   singular
   total = total + 1
   fraction = singular/total
   disp('Hit <ctrl-C> to stop')
   pause(1)
end

