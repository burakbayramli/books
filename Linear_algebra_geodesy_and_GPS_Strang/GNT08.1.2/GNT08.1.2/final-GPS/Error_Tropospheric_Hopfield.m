%This Function approximate Troposspheric Group Delay Base on 
%application . edited by B. Parkinson,J. Spilker, P.Enge, AIAA,1996
%CopyRight By Moein Mehrtash
%**************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/21/2008              *
% Email: moeinmehrtash@yahoo.com                                          *
%**************************************************************************
% Reference:"GPS Theory and application",edited by B.Parkinson,J.Spilker, *
%**************************************************************************           
%Input
%        T_amb:'C =>At reciever antenna location
%        P_amb:hPa =>At reciever antenna location
%        P_vap:hPa =>Water vapore pressure at reciever antenna location
%        Pos_Rcv       : XYZ position of reciever               (Meter) 
%        Pos_SV        : XYZ matrix position of GPS satellites  (Meter) 

%Output:    
%        Delta_R_Trop: m =>Tropospheric Error Correction
%**************************************************************************           
function Delta_R_Trop=Error_Tropospheric_Hopfield(T_amb,P_amb,P_vap,Pos_Rcv,Pos_SV)
S=size(Pos_SV);
m=S(1);n=S(2);
for i=1:m
  [E,A0]=Calc_Azimuth_Elevation(Pos_Rcv,Pos_SV(i,:));
  El(i)=E;                                                   %Elevation Rad
  A(i)=A0;                                                    %Azimoth Rad
end

%Zenith Hydrostatic Delay
Kd=1.55208*10^(-4)*P_amb*(40136+148.72*T_amb)/(T_amb+273.16);

%Zenith Wet Delay
Kw=-.282*P_vap/(T_amb+273.16)+8307.2*P_vap/(T_amb+273.16)^2;

for i=1:m
  Denom1(i)=sin(sqrt(El(i)^2+1.904*10^-3));
  Denom2(i)=sin(sqrt(El(i)^2+.6854*10^-3));
  %Troposhpheric Delay Correctoion
  Delta_R_Trop(i)=Kd/Denom1(i)+Kw/Denom2(i);                        % Meter
end