%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Main Part of The Program, Which asemble diffrent Matlab Function
clc
format long e
clear
close all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Initial Constant
C=299792458;
F=-4.442807633*10^(-10);
T_amb=20;
P_amb=101;
P_vap=.86;
color=['r','b','c','g','k','m'];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Load Data and Difinition
load project_data.mat
GPS_Time=iono(1);Alpha=iono(2:5);
Beta=iono(6:9);
af=[eph(21,:);eph(20,:);eph(19,:)]';

Toc=[eph(18,:)];
Ttr=GPS_Time-pr./C;

ec=[eph(5,:)];
A=[eph(7,:)].^2;
Tgd=[eph(17,:)];

Rho_0=pr;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Compute satellite position and corrected orbit value
[Pos_xyz_Mat,Orbit_parameter]=SV_Ephemeris_Model(iono(1,1),eph);
Pos_SV=Pos_xyz_Mat;   
     E=Orbit_parameter(2,:);
     A=Orbit_parameter(9,:);
     Ec=Orbit_parameter(10,:);
     Inc=Orbit_parameter(7,:);
     Omega=Orbit_parameter(8,:);
     v=Orbit_parameter(3,:);
Dim=size(Pos_SV);
n=Dim(1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%Plot The Geometry%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
plot_Orbit(Orbit_parameter,color)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Initial GPS Position
Xu=0;Yu=0;Zu=0;Pos_Rcv=[Xu Yu Zu];
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Start Iteration
%Initialization Parameter
  Iter=1;
  Pos_Rcv_N{Iter}=[Pos_Rcv];
%Constrain for convergence  
  B1=1;
  END_LOOP=100;
while (END_LOOP > B1) 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%Compute Satellite Pseudo Range%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Rho=Rho_0+Delta_I+Delta_T+Delta_Rel+Delta_Off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%A.Compute Satellite Offset Clock Error
dTclk_Offset=Error_Satellite_Clock_Offset(af,Ttr,Toc);               %(Sec)
dR_Error_Satellite_Clock_Offset=C*dTclk_Offset;                    %(Meter)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%B.Compute Satellite Relavistic Clock Error
dTclk_Rel=Error_Satellite_Clock_Relavastic(F,ec,A,E,Tgd);            %(Sec)
dR_Error_Satellite_Clock_Rel=C*dTclk_Rel;                          %(Meter)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%C.Compute IonoSphere Error
dT_Ion=Error_Ionospheric_Klobuchar(Pos_Rcv_N{Iter},Pos_SV,Alpha,Beta,GPS_Time);%(Sec)
dR_Error_Ionsphere=C*dT_Ion;                                       %(Meter)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%D.Compute Tropospheric Error
Delta_R_Trop=Error_Tropospheric_Hopfield(T_amb,P_amb,P_vap,Pos_Rcv_N{Iter},Pos_SV);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Compute Pseudo Range
Rho=Rho_0'+dR_Error_Satellite_Clock_Offset'+dR_Error_Satellite_Clock_Rel';
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Compute Covariance Matrix,deltax
[G0,Delta_X0,Pos_Rcv_N0,B0]=Gen_G_DX_XYZ_B(Pos_SV,Pos_Rcv_N{Iter},Rho);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Iter=Iter+1
G{Iter}=G0;
Delta_X{Iter}=Delta_X0;
B(Iter)=B0;
Pos_Rcv_N{Iter}=Pos_Rcv_N0;
END_LOOP=norm(Delta_X0(1:3));
end %End of While

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Plot Resualt
for i=1:Iter-1
    a=Delta_X{i+1}
    dx1(i)=a(1);
    dx2(i)=a(2);
    dx3(i)=a(3);
    de(i)=sqrt(a(1)^2+a(2)^2+a(3)^2);
end
for i=1:Iter
    Pos_R=Pos_Rcv_N{i};
    x1(i)=Pos_R(1);
    x2(i)=Pos_R(2);
    x3(i)=Pos_R(3);

end
a=Pos_Rcv_N{Iter};
plot3(a(1),a(2),a(3),'*')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %plot Delta X
% figure
% subplot(3,1,1)
% plot(dx1)
% grid on
% ylabel('\Delta_X')
% title('Covariance Solution')
% hold on
% subplot(3,1,2)
% plot(dx2)
% grid on
% ylabel('\Delta_Y')
% subplot(3,1,3)
% plot(dx3)
% grid on
% ylabel('\Delta_Z')
% xlabel('Iterration')
% figure
% plot(de)
% title('GPS Error Position')
% xlabel('Iteration')
% ylabel('Norm Error')
% 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %GPS reciever Position
% figure
% subplot(3,1,1)
% plot(x1)
% title('GPS Reciever Postion')
% grid on
% ylabel('X')
% hold on
% subplot(3,1,2)
% plot(x2)
% grid on
% ylabel('Y')
% subplot(3,1,3)
% plot(x3)
% grid on
% ylabel('_Z')
% xlabel('Iterration')
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% figure
% plot(B)
% title('GPS Reciever Clock Bias')
% xlabel('Iteration')
% ylabel('GPS Reciever Clock Bias (Sec)')
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% G0=G{Iter};
% GTG=G0'*G0;
% GDOP=sqrt(trace(GTG))
% PDOP=sqrt(GTG(1,1)+GTG(2,2)+GTG(3,3))
% HDOP=sqrt(GTG(1,1)+GTG(2,2))
% VDOP=sqrt(GTG(1,1))
% TDOP=sqrt(GTG(4,4))
% 
% 
