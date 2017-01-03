% Perform covariance analysis by solving Riccati equations
% for one time iteration. 
%
% Solve Riccati equation
%
Pminus = phi*Pplus_last*phi' + Q;      % Predicted performance
Kbar   = Pminus*H'/(H*Pminus*H' + R);  % Kalman gain
Pplus  = (eye(5)-Kbar*H)*Pminus;       % Corrected performance
%
% Output vectors for plotting
%
   time(k)=t;
   for i=1:5;
      Pminus_out(i,k)    = sqrt(Pminus(i,i)); % RMS predicted uncertainties
      Pplus_out(i,k)     = sqrt(Pplus(i,i));  % RMS corrected uncertainties
      for j=1:4;
         Kbar_out(i,j,k) = Kbar(i,j);         % Kalman gains
      end
   end;   
%
% Initialize next time step
%
Pplus_last = Pplus;
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  




