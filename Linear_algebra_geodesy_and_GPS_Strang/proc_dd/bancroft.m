function pos = bancroft(B_pass)
%BANCROFT Calculation of preliminary coordinates
%   	    for a GPS receiver based on pseudoranges
%	       to 4 or more satellites. The ECEF
%	       coordinates (see function e_r_corr)
%	       are the first three elements of
%	       each row of B. The fourth element of each
%	       row of B contains the observed pseudorange.
%	       Each row pertains to one satellite.
%	       The pseudorange in the first row of B is
%	       used to descriminate between the two
%	       possible solutions.

%Reference: Bancroft, S. (1985) An Algebraic Solution
%  		       of the GPS Equations, IEEE Trans. Aerosp.
%	   	       and Elec. Systems, AES-21, 56--59

%pos(1) = X
%pos(2) = Y
%pos(3) = Z
%pos(4) = c*dt

%Kai Borre 04-30-95, improved by C.C. Goad 11-24-96
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

% Test values to use in debugging
% B_pass =[ -11716227.778 -10118754.628	21741083.973 22163882.029;
%     	   -12082643.974 -20428242.179	11741374.154 21492579.823;
%	          14373286.650 -10448439.349	19596404.858 21492492.771;
%	          10278432.244 -21116508.618  -12689101.970 25284588.982];
% Solution:    595025.053  -4856501.221	 4078329.981

% Test values to use in debugging
% B_pass = [14177509.188  -18814750.650   12243944.449  21119263.116;
%           15097198.146   -4636098.555   21326705.426  22527063.486;
%	         23460341.997   -9433577.991    8174873.599  23674159.579;
%	         -8206498.071  -18217989.839   17605227.065  20951643.862;
%	          1399135.830  -17563786.820   19705534.862  20155386.649;
%  	       6995655.459  -23537808.269   -9927906.485  24222112.972];
% Solution: 	596902.683   -4847843.316    4088216.740

v_light = 299792458;
pos = zeros(4,1);

for iter = 1:2
   B = B_pass;
   [m,n] = size(B);
   for i = 1:m
      x = B(i,1);
      y = B(i,2);
      if iter == 1
         traveltime = 0.072;
      else
         z = B(i,3);
         rho = (x-pos(1))^2+(y-pos(2))^2+(z-pos(3))^2;
         traveltime = sqrt(rho)/v_light;
      end
      angle = traveltime*7.292115147e-5;
      cosa = cos(angle);
      sina = sin(angle);
      B(i,1) =	cosa*x + sina*y;
      B(i,2) = -sina*x + cosa*y;
   end; % i-loop
   
   if m > 4
      BBB = inv(B'*B)*B';
   else
      BBB = inv(B);
   end
   e = ones(m,1);
   alpha = zeros(m,1);
   for i = 1:m
      alpha(i) = lorentz(B(i,:)',B(i,:)')/2; 
   end
   BBBe = BBB*e;
   BBBalpha = BBB*alpha;
   a = lorentz(BBBe,BBBe);
   b = lorentz(BBBe,BBBalpha)-1;
   c = lorentz(BBBalpha,BBBalpha);
   root = sqrt(b*b-a*c);
   r(1) = (-b-root)/a;
   r(2) = (-b+root)/a;
   possible_pos = zeros(4,2);
   for i = 1:2
      possible_pos(:,i) = r(i)*BBBe+BBBalpha;
      possible_pos(4,i) = -possible_pos(4,i);
   end
   for j =1:m
      for i = 1:2
         c_dt = possible_pos(4,i);
         calc = norm(B(j,1:3)' -possible_pos(1:3,i))+c_dt;
         omc = B(j,4)-calc;
         abs_omc(i) = abs(omc);
      end
   end; % j-loop
   
   % discrimination between roots
   if abs_omc(1) > abs_omc(2)
      pos = possible_pos(:,2);
   else 
      pos = possible_pos(:,1);
   end
end; % iter loop
%%%%%%%%%%%%  end bancroft.m  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
