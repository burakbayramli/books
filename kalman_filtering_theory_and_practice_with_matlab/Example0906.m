% 
% Grewal & Andrews, Kalman Filtering: Theory and Practice Using MATLAB, 
% John Wiley & Sons, 2008 
% 
% 
% Example 9.6: Effects of satellite geometry 
% 
% Requests the azimuths and elevation angles to four satellites 
% Simulates 10 minutes of filtering with that geometry. 
% Plots histories of RMS position and clock uncertainties 
% 
% Assumes 15 meter RMS pseudorange error 
% Assumes 10^(-8) relative clock stability over 1 second 
% 
close all, 
clear all, 
% 
Dt          = 1;  % Discrete time-step 
% 
P0          = zeros(5); 
P0(1,1)     = 1e6; 
P0(2,2)     = 1e6; 
P0(3,3)     = 1e6; 
P0(4,4)     = 9e6; 
P0(5,5)     = 9e2; % Initial covariance of state estimation uncertainty 
% 
Phi = zeros(5); 
Phi(1,1)    = 1; 
Phi(2,2)    = 1; 
Phi(3,3)    = 1; 
Phi(4,4)    = 1; 
Phi(4,5)    = Dt; 
Phi(5,5)    = 1; % State transition matrix 
% 
Q           = zeros(5); 
Q(4,4)      = 6; 
Q(4,5)      = 5; 
Q(5,4)      = Q(4,5); 
Q(5,5)      = 10; % Dynamic disturbance noise covariance 
% 
R           = 225*eye(4); % Covariance of measurement noise 
r           = 225;        % Diagonal term for serial processing 
% 
H           = zeros(4,5); 
H(1,4)      = 1; 
H(2,4)      = 1; 
H(3,4)      = 1; 
H(4,4)      = 1; 
for CaseNo=1:10; 
    % 
    % Input the four satellite directions 
    % 
    disp('Input 4 satellite elevation angles and azimuths IN DEGREES.'); 
    disp('Azimuth angle (0 to 360 DEG) is measured clockwise from north'); 
    disp('Elevation angle (0 to 90 DEG) is measured upward from the horizon'); 
    disp('   Entering a NEGATIVE ELEVATION ANGLE terminates the program.'); 
    for SatNo=1:4, 
        Elev(SatNo) = input(['Elevation angle for satellite number ',num2str(SatNo),'>']); 
        if Elev(SatNo)<0 
            return; 
        end; 
        Azim(SatNo) = input(['Azimuth angle for satellite number ',num2str(SatNo),'>']); 
    end; 
    NoErr       = [P0(1,1)]; 
    EaErr       = [P0(2,2)]; 
    DoErr       = [P0(3,3)]; 
    CbErr       = [P0(4,4)]; 
    CdErr       = [P0(5,5)]; 
    t           = [0]; 
    phi1    = Elev(1)*pi/180; 
    sphi1   = sin(phi1); 
    cphi1   = cos(phi1); 
    H(1,3)  = sphi1; 
    theta1   = Azim(1)*pi/180; 
    stheta1  = sin(theta1); 
    ctheta1  = cos(theta1); 
    H(1,1)   = -ctheta1*cphi1; 
    H(1,2)   = -stheta1*cphi1; 
    phi2    = Elev(2)*pi/180; 
    sphi2   = sin(phi2); 
    cphi2   = cos(phi2); 
    H(2,3)  = sphi2; 
    theta2   = Azim(2)*pi/180; 
    stheta2  = sin(theta2); 
    ctheta2  = cos(theta2); 
    H(2,1)   = -ctheta2*cphi2; 
    H(2,2)   = -stheta2*cphi2; 
    phi3    = Elev(3)*pi/180; 
    sphi3   = sin(phi3); 
    cphi3   = cos(phi3); 
    H(3,3)  = sphi3; 
    theta3   = Azim(3)*pi/180; 
    stheta3  = sin(theta3); 
    ctheta3  = cos(theta3); 
    H(3,1)   = -ctheta3*cphi3; 
    H(3,2)   = -stheta3*cphi3; 
    phi4    = Elev(4)*pi/180; 
    sphi4   = sin(phi4); 
    cphi4   = cos(phi4); 
    H(4,3)  = sphi4; 
    theta4   = Azim(4)*pi/180; 
    stheta4  = sin(theta4); 
    ctheta4  = cos(theta4); 
    H(4,1)   = -ctheta4*cphi4; 
    H(4,2)   = -stheta4*cphi4; 
    P        = P0; 
    for j=1:4, 
        h   = H(j,:); 
        PHT = P*h'; 
        K   = PHT/(h*PHT+r); 
        P   = P - K*PHT'; 
    end; 
    P   = (P+P')/2; 
    NoErr       = [NoErr,P(1,1)]; 
    EaErr       = [EaErr,P(2,2)]; 
    DoErr       = [DoErr,P(3,3)]; 
    CbErr       = [CbErr,P(4,4)]; 
    CdErr       = [CdErr,P(5,5)]; 
    t           = [t,0]; 
    % 
    for k=1:60, % one minute of filtering 
        P   = Phi*P*Phi'+Q; 
        NoErr       = [NoErr,P(1,1)]; 
        EaErr       = [EaErr,P(2,2)]; 
        DoErr       = [DoErr,P(3,3)]; 
        CbErr       = [CbErr,P(4,4)]; 
        CdErr       = [CdErr,P(5,5)]; 
        t           = [t,k]; 
        for j=1:4, 
            h   = H(j,:); 
            PHT = P*h'; 
            K   = PHT/(h*PHT+r); 
            P   = P - K*PHT'; 
        end; 
        P   = (P+P')/2; 
        NoErr   = [NoErr,P(1,1)]; 
        EaErr   = [EaErr,P(2,2)]; 
        DoErr   = [DoErr,P(3,3)]; 
        CbErr   = [CbErr,P(4,4)]; 
        CdErr   = [CdErr,P(5,5)]; 
        t       = [t,k]; 
    end; 
    semilogy(t,sqrt(NoErr),'r-',t,sqrt(EaErr),'b-',t,sqrt(DoErr),'k-'); 
    title('RMS Position Uncertainty VS Time'); 
    xlabel('Time [sec]'); 
    ylabel('RMS Uncertainty [m]'); 
    legend('North','East','Vertical'); 
    disp('Press <ENTER> to continue.'); 
    pause 
    semilogy(t,sqrt(CbErr),'r-',t,sqrt(CdErr),'b-'); 
    title('RMS Clock Uncertainties VS Time'); 
    xlabel('Time [sec]'); 
    ylabel('RMS Uncertainty]'); 
    legend('Bias [m]','Drift [m/s]'); 
    disp('Press <ENTER> to continue.'); 
    pause; 
end; 
