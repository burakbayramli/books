function testSteel
% testSteel  Verify that solutions obtained by ode45 are independent of
%            interpolative refinement.  Solution is obtained with default
%            refinement (= 4) and with no refinement.
%
% Synopsis:  testSteel
%
% Input:  none
%
% Output:  Every fourth time step from refined solution is printed along with 
%          all steps from unrefined solution. Solutions are identical.

close all                    %  close any plot windows
[t4,T4] = steelHeat;         %  solution with default refinement = 4
[t1,T1] = steelHeat(1);      %  solution with no refinement
n = length(t4);
disp('    time      T1        time      T4');
disp([t1  T1  t4(1:4:n)    T4(1:4:n)])
