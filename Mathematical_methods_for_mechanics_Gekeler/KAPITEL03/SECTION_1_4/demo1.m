function demo1
% Eckart Gekeler, Universitaet Stuttgart, Release 17.4.05
% Example cf. Spellucci, p. 117
% Example 1: Steepest descend
% Example 2: BFGS Method
% -------------------------------------------
clc, format short, format compact
disp(' Run both examples then call bild01 ')
bsp = 100; KK = [1,2];
while ~ismember(bsp,KK)
   bsp = input(' desc/bfgs ? (1/2) ');
end
switch bsp
case 1
   TOL  = 1.0E-03;
   X    = zeros(2,1);
   G1   = desc('bsp01',X,TOL);
   save daten1 G1
case 2
   TOL  = 1.0E-03;
   X    = zeros(2,1);
   G2   = bfgs('bsp01',X,TOL);
   save daten2 G2
end
