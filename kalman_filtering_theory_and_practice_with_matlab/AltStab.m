disp('Kalman Filtering: Theory and Practice Using MATLAB'); 
disp('M. S, Grewal & A. P. Andrews'); 
disp('Wiley, 2008'); 
disp(' '); 
disp('Kalman filter implementation and demonstration for INS vertical'); 
disp('channel stabilization using GNSS-derived altitude.'); 
disp(' '); 
disp('The program will step through 10-minute simulations of aided and'); 
disp('unaided altitude errors, with simulated RMS accelerometer noise ='); 
disp([num2str(1e-6),', ',num2str(1e-5),', ',num2str(1e-4),', ',num2str(1e-3),', ',num2str(1e-2),', ',num2str(1e-1),' and 1g,']); 
disp('pausing 10 seconds to display results between cases.'); 
disp(' '); 
disp('The final plot is of the a priori and a posteriori RMS altitude'); 
disp('uncertainties from the Kalman filter covariances---for all cases.'); 
disp(' '); 
disp('Press ENTER key to continue.') 
pause; 
disp('...resuming.'); 
% 
close all; 
clear all; 
% 
Dt   = 1;      % Discrete time step 
tauS = 806.4;  % Schuler time-constant for vertical channel 
tauA = 300;    % 5-minute time-constant for accelerometer error variation 
F(1,1) = 0; 
F(1,2) = 1; 
F(1,3) = 0; 
F(2,1) = 1 / tauS^2; 
F(2,2) = 0; 
F(2,3) = 1; 
F(3,1) = 0; 
F(3,2) = 0; 
F(3,3) = -1 / tauA; 
% 
H      = [1,0,0]; 
r      = 400;      % 20-meter RMS GPS vertical error 
% 
Phi    = expm(Dt*F); 
% 
P11inf = []; 
SigmaG = []; 
N      = 0; 
for Log10sigmaG=-6:0, 
    N      = N + 1; 
    sigmaG = 10^Log10sigmaG; 
    SigmaG = [SigmaG,sigmaG]; 
    sigmaA = sigmaG*9.8; 
    Qt     = 2*sigmaA^2/tauA; 
    % 
    % The following vector of polynomial coefficients is for a 10th-order 
    % polynomial, one of the solutions to which is the steady-state mean- 
    % squared altitude uncertainty from GNSS-aided INS altitude 
    % stabilization for each value of RMS vertical accelerometer error. 
    % 
    % This polynomial solution comes from solving the steady-state Riccati 
    % equation in discrete time. 
    % 
    PC = [tauA^4*tauS^6; % polynomial coefficients of 10th-order polynomial 
        0; 
        -10*r^2*tauA^4*tauS^4 - 2*tauA^4*tauS^6*r^2; 
        0; 
        32*r^4*tauA^4*tauS^2 + 16*tauA^4*tauS^4*r^4 - 32*r^4*tauS^6*tauA^2; 
        -64*r^5*tauS^6*tauA; 
        -32*tauA^4*tauS^2*r^6 + 128*r^6*tauS^4*tauA^2 - 32*r^6*tauS^6 - 64*tauA^4*tauS^6*Qt*r^5 - 32*r^6*tauA^4; 
        -128*tauA^3*tauS^6*Qt*r^6 + 256*r^7*tauS^4*tauA; 
        -64*tauA^2*tauS^6*Qt*r^7 + 128*tauA^4*tauS^6*r^7*Qt + 128*r^8*tauS^4 + 128*r^7*tauA^4*Qt*tauS^4; 
        256*tauA^3*tauS^6*r^8*Qt + 256*r^8*tauA^3*Qt*tauS^4; 
        128*tauA^2*tauS^6*r^9*Qt + 128*r^9*tauA^2*Qt*tauS^4]; 
    P11inf = [P11inf,roots(PC)]; % one of its roots is P(1,1) at steady-state 
    Q      = zeros(3); 
    Q(3,3) = Dt*Qt; 
    P      = zeros(3); 
    P(1,1) = 10000;      % 100 m RMS initial INS altitude error 
    P(2,2) = 100;        % 10 m/s RMS initial INS altitude rate error 
    P(3,3) = sigmaA^2;   % sigmaA is RMS accelerometer error 
    k      = 0; 
    x      = [sqrt(P(1,1))*randn;sqrt(P(2,2))*randn;sqrt(P(3,3))*randn]; 
    xu     = x; % unaided altitude error 
    for T=1:601, % 10-minute run 
        k    = k + 1; 
        t(k) = T-1; 
        z    = sqrt(r)*randn; 
        SigmaH(N,k) = sqrt(P(1,1)); 
        PHT  = P*H'; 
        K    = PHT/(H*PHT + r); 
        x    = x + K*(z - H*x); 
        err(k) = xu(1); 
        alt(k) = x(1); 
        P    = P - K*PHT'; 
        P    = .5*(P+P'); 
        k    = k + 1; 
        t(k) = T-1; 
        da   = sqrt(Q(3,3))*randn; 
        x    = Phi*x + [0;0;da]; 
        xu   = Phi*xu + [0;0;da]; 
        alt(k) = x(1); 
        err(k) = xu(1); 
        SigmaH(N,k) = sqrt(P(1,1)); 
        P    = Phi*P*Phi' + Q; 
    end; 
    subplot(2,1,1); 
    plot(t,alt); 
    title(['GNSS-aided INS Alt. Stabiliization with RMS Accelerometer Noise = ',num2str(sigmaG),' [G]']); 
    xlabel('Time [sec]'); 
    ylabel('Aided Alt. Err. [m]'); 
    subplot(2,1,2); 
    plot(t,err); 
    xlabel('Time [sec]'); 
    ylabel('Unaided Alt. Err. [m]'); 
    pause(10); 
end; 
subplot(1,1,1), 
plot(t,SigmaH); 
legend('10^{-6}g','10^{-5}g','10^{-4}g','10^{-3}g','10^{-2}g','10^{-1}g','1g'); 
text(10,90,'RMS ACCELEROMETER NOISE','HorizontalAlignment','left','VerticalAlignment','middle'); 
text(100,86,'LISTED IN LEGEND AT RIGHT','HorizontalAlignment','left','VerticalAlignment','middle'); 
text(10,80,'ACCELEROMETER NOISE CORRELATION TIME','HorizontalAlignment','left','VerticalAlignment','middle'); 
text(100,76,'5 MINUTES','HorizontalAlignment','left','VerticalAlignment','middle'); 
text(10,70,'RMS GNSS ALTITUDE ERROR','HorizontalAlignment','left','VerticalAlignment','middle'); 
text(100,66,'20 METERS','HorizontalAlignment','left','VerticalAlignment','middle'); 
xlabel('Time [sec]'); 
ylabel('RMS Altitude Estimation Uncertainty [m]'); 
title('GNSS-aided INS Altitude Stabilization'); 
% 
disp(' '); 
disp('Entering ''P11inf'' on the keyboard will display the P(1,1) polynomial'); 
disp('roots [rows] for the different RMS accelerometer noise [columns].'); 
disp('One of these roots is the steady-state mean-squared altitude'); 
disp('uncertainty.  The correct solution is real and positive, and not the'); 
disp('largest value, which is constant.'); 
% 
