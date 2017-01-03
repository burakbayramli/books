%KLEUS  Explicit method for computing preliminary receiver
%       coordinates from four pseudoranges and ECEF coordinates 
%       of four satellites

% Reference:
%   Kleusberg, A. (1994): Die direkte L\"osung des
% 	    r\"aumlichen Hyperbelschnitts. Zeitschrift
%      f\"ur Vermessungswesen, pp. 188--192

%Kai Borre 05-20-97
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/10/15  $

% Original Kleusberg data
% obs = [  14832308.66	-20466715.89  -7428634.75;
%     	 -15799854.05  -13301129.17  17133838.24;
%	         1984818.91  -11867672.96  23716920.13;
%	       -12480273.19  -23382560.53   3278472.68];
% d = [ -1396163.280;
%	     -3681954.659;
%	      -888386.092];
tic
% Test values to use in debugging
obs = [  -11716227.778 -10118754.628	21741083.973 22163882.029;
         -12082643.974 -20428242.179	11741374.154 21492579.823;
          14373286.650 -10448439.349	19596404.858 21492492.771;
          10278432.244 -21116508.618  -12689101.970 25284588.982];
% Solution:   595025.053  -4856501.221	 4078329.981

diff = obs(:,4)-obs(1,4);
d = diff(2:4,1);

% Test values to use in debugging
%  B_pass = [ 14177509.188  -18814750.650   12243944.449  21119263.116;
%     	     15097198.146   -4636098.555   21326705.426  22527063.486;
%	           23460341.997   -9433577.991    8174873.599  23674159.579;
%     	     -8206498.071  -18217989.839   17605227.065  20951643.862;
%	            1399135.830  -17563786.820   19705534.862  20155386.649;
%	            6995655.459  -23537808.269   -9927906.485  24222112.972];
% Solution:	596902.683   -4847843.316    4088216.740

b = [norm(obs(2,1:3)-obs(1,1:3));
     norm(obs(3,1:3)-obs(1,1:3));
     norm(obs(4,1:3)-obs(1,1:3))];
e1 = (obs(2,1:3)-obs(1,1:3))'/b(1);
e2 = (obs(3,1:3)-obs(1,1:3))'/b(2);
e3 = (obs(4,1:3)-obs(1,1:3))'/b(3);
F1 = b(1)/(b(1)^2-d(1)^2)*e1 - b(2)/(b(2)^2-d(2)^2)*e2;
F2 = b(2)/(b(2)^2-d(2)^2)*e2 - b(3)/(b(3)^2-d(3)^2)*e3;
f1 = F1/norm(F1);
f2 = F2/norm(F2);

A = [f1'; f2'];
u = [ (d(2)/(b(2)^2-d(2)^2)-d(1)/(b(1)^2-d(1)^2))/norm(F1);
      (d(3)/(b(3)^2-d(3)^2)-d(2)/(b(2)^2-d(2)^2))/norm(F2)];
g = cross(f1,f2);
h = u(2)*f1-u(1)*f2;
E1 = (cross(g,h) + g*sqrt((norm(g))^2 - (norm(h))^2))/(norm(g))^2;
E2 = (cross(g,h) - g*sqrt((norm(g))^2 - (norm(h))^2))/(norm(g))^2;
s1 = (b(1)^2-d(1)^2)/(2*(d(1)+b(1)*sum(E1.*e1)));
s2 = (b(1)^2-d(1)^2)/(2*(d(1)+b(1)*sum(E2.*e1)));

format bank
x2 = obs(1,1:3)'+s2*E1;
x4 = obs(1,1:3)'+s2*E2
toc
%%%%%%%%%%% end kleus.m  %%%%%%%%%%%%%%%%%%%%%%
