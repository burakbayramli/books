%This Function approximate Ionospheric Group Delay 
%CopyRight By Moein Mehrtash
%************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/21/2008            *
% Email: moeinmehrtash@yahoo.com                                        *
%************************************************************************           
% ***********************************************************************           
%      Function for   computing an Ionospheric range correction for the *
%      GPS L1 frequency from the parameters broadcasted in the GPS      *
%      Navigation Message.                                              *
%      ==================================================================
%      References:                                                      *
%      Klobuchar, J.A., (1996) "Ionosphercic Effects on GPS", in        *
%        Parkinson, Spilker (ed), "Global Positioning System Theory and *
%        Applications, pp.513-514.                                      *
%      ICD-GPS-200, Rev. C, (1997), pp. 125-128                         *
%      NATO, (1991), "Technical Characteristics of the NAVSTAR GPS",    *
%        pp. A-6-31   -   A-6-33                                        *
%      ==================================================================
%    Input :                                                            *
%        Pos_Rcv       : XYZ position of reciever               (Meter) *
%        Pos_SV        : XYZ matrix position of GPS satellites  (Meter) *
%        GPS_Time      : Time of Week                           (sec)   *
%        Alfa(4)       : The coefficients of a cubic equation           *
%                        representing the amplitude of the vertical     *
%                        dalay (4 coefficients - 8 bits each)           *
%        Beta(4)       : The coefficients of a cubic equation           *
%                        representing the period of the model           *
%                        (4 coefficients - 8 bits each)                 *
%    Output:                                                            *
%       Delta_I        : Ionospheric slant range correction for         *
%                        the L1 frequency                       (Sec)   *
%     ==================================================================

function [Delta_I]=Error_Ionospheric_Klobuchar(Pos_Rcv,Pos_SV,Alpha,Beta,GPS_Time)
GPS_Rcv = ECEF2GPS(Pos_Rcv);
Lat=GPS_Rcv(1)/pi;Lon=GPS_Rcv(2)/pi;   % semicircles unit Lattitdue and Longitude 
S=size(Pos_SV);
m=S(1);n=S(2);

for i=1:m
[El,A0]=Calc_Azimuth_Elevation(Pos_Rcv,Pos_SV(i,:));
E(i)=El/pi;                                            %SemiCircle Elevation
A(i)=A0;                                               %SemiCircle Azimoth 
% Calculate the Earth-Centered angle, Psi
Psi(i)=0.0137/(E(i)+.11)-0.022;                        %SemiCircle

%Compute the Subionospheric lattitude, Phi_L
Phi_L(i)=Lat+Psi(i)*cos(A(i));                         %SemiCircle
if Phi_L(i)>0.416
    Phi_L(i)=0.416;
elseif Phi_L(i)<-0.416
    Phi_L(i)=-0.416;
end

%Compute the subionospheric longitude, Lambda_L
Lambda_L(i)=Lon+(Psi(i)*sin(A(i))/cos(Phi_L(i)*pi));  %SemiCircle

%Find the geomagnetic lattitude ,Phi_m, of the subionospheric location
%looking toward each GPS satellite:
Phi_m(i)=Phi_L(i)+0.064*cos((Lambda_L(i)-1.617)*pi);

%Find the Local Time ,t, at the subionospheric point
t(i)=4.23*10^4*Lambda_L(i)+GPS_Time;                 %GPS_Time(Sec)
if t(i)>86400
    t(i)=t(i)-86400;
elseif t(i)<0
    t(i)=t(i)+86400;
end

%Convert Slant time delay, Compute the Slant Factor,F
F(i)=1+16*(.53-E(i)^3);

%Compute the ionospheric time delay T_iono by first computing x
Per(i)=Beta(1)+Beta(2)*Phi_m(i)+Beta(3)*Phi_m(i)^2+Beta(4)*Phi_m(i)^3;
if Per(i) <72000                                     %Period
    Per(i)=72000;
end
x(i)=2*pi*(t(i)-50400)/Per(i);                       %Rad
AMP(i)=Alpha(1)+Alpha(2)*Phi_m(i)+Alpha(3)*Phi_m(i)^2+Alpha(4)*Phi_m(i)^3;
if AMP(i)<0 
    AMP(i)=0
end
if abs(x(i))>1.57
    T_iono(i)=F(i)*5*10^(-9);
else
    T_iono(i)=F(i)*(5*10^(-9)+AMP(i)*(1-x(i)^2/2+x(i)^4/4));
end

end%for

Delta_I=T_iono;
