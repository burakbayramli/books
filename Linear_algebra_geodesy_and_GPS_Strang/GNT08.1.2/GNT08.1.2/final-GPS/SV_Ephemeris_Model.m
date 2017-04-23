%This Function use Ephemeris Data and Calculate satellite Position 
%CopyRight By Moein Mehrtash
%**************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/28/2008              *
% Email: moeinmehrtash@yahoo.com                                          *
%**************************************************************************
%**************************************************************************
% Satellite Position By Ephemeris Model 
%Function's Inputs:
    % GPS_RVC_T:(Sec):
    % A:semi-major orbit axis (meter)
    % n0: Compute mean motion (rad/s)
    % tk:time from ephemeris reference epoch
    % n: corrected mean motion
    % Dealta_n:correction for mean motion
    % M0:mean Anomaly at refernce time
    % Mk:Mean anomaly
    % Ek: Keplers equation for the eccentric anomaly Ek (rad)
    % e: orbit eccentricity
    % Toe: Refernce time of ephemeris parameters
    % i0: inlination angle at reference time
    % Phik:Argument of lattitude
    % Omega0: Longitude of Ascending Node
    % omega: Argument of perigee (semicicrcle)
    % Omegadot: rate of right ascension
    % IDOT: rate of inlination angle
    % Delta_uk:Argument of lattitude correction
    % Delta_rk:Argument of radius correction
    % Delta_ik:Argument of inclination correction
    % uk:Corrcted argument of lattitude
    % rk: corrected radius
    % ik:corrected inclination
    % xpk:satellite position in orbital plane
    % ypk:satellite position in orbital plane
    % SEcond Harmonic Perturbation coefiicient
        %C_us:Amplitude of the cosine harmonic correction term to the argument
        %     of lattitude(Rad)
        %C_uc:Amplitude of the sine harmonic correction term to the argument
        %     of lattitude(Rad)
        %C_rs:Amplitude of the cosine harmonic correction term to orbit
              %radius(meter)
        %C_rc:Amplitude of the sine harmonic correction term to orbit
              %radius(meter)
        %C_is:Amplitude of the cosine harmonic correction term to the angle of
        %     inclination(Rad)
        %C_ic:Amplitude of the sine harmonic correction term to the angle of
        %     inclination(Rad)

  
%Function's Outputs:
    % xk,yk,zk: Satellite position in ECEF
    % Ek:Eccentric Anomaly
    % A:semi-major orbit axis
    % e: orbit eccentricity
    % ik: Corrected Inlination
    % Omegak: Corrected longitude of ascending node
    % Nuk:True anomaly as a function of the eccentric anomaly
    %Function's Constants:
    % Mu:WGS-84 value of the earths universal gravitational 
    %    parameter(3.986005*10^14 m4/s2)
    % Omega_dote:WGS-84 value of the earths roration rate 
    %           (7.2921151467*10^(-5)rad/s)
%Input:(GPS_RVC_T,[C_rs Dealta_n M0 C_uc e C_us sqrt(A) Toe C_ic Omega0 C_is
%                  i0 C_rc omega Omegadot IDOT]     

%************************************************************************** 
function [Pos_xyz_Mat,Orbit_parameter]=SV_Ephemeris_Model(GPS_RVC_T,data);

[Parameter,ST_NO]=size(data);
% Constant Parameter of this function
Mu=3.986005*10^14;
Omega_dote=7.2921151467*10^(-5);

for ST_No=1:ST_NO

%Load Data 
A=data(7,ST_No)*data(7,ST_No);
t=GPS_RVC_T;
toe=data(8,ST_No);
Delta_n=data(2,ST_No);
M0=data(3,ST_No);
omega=data(14,ST_No);
C_us=data(6,ST_No);
C_uc=data(4,ST_No);
C_rs=data(1,ST_No);
C_rc=data(13,ST_No);
C_is=data(11,ST_No);
C_ic=data(9,ST_No);
i0=data(12,ST_No);
IDOT=data(16,ST_No);
Omega0=data(10,ST_No);
Omegadot=data(15,ST_No);
e=data(5,ST_No);

%************************************************************************** 
%************************************************************************** 
%Compute Time From Ephemeris Refernce Epoch
%Input:
    %t(sec):GPS Sys. Time
    %toe(sec):Reference Time Ephemeris
%Output:
    %tk(sec):Time from ephemeris reference epoch
tk=t-toe;
%************************************************************************** 
%************************************************************************** 
%Compute corrected mean motion
%Input:
    %A(meter): Semi-major axis
    %Mu(meter^3/sec^2): value of Earth's universal gravitational parameters
    %tk(sec):Time from Ephemeris Reference Epoch
    %Delta_n(semi-circle/sec):Mean Motion Diference From Computed Value
    %M0(semi-circles):
%Output:
    %n0(Rad/sec):Computed mean motion 
    %n(semi-circle/sec):Corrected mean motion
    %Mk(semi-circle):Mean anomaly
n0=sqrt(Mu/(A^3));
n=n0/pi+Delta_n;
Mk=M0+n*tk;

%************************************************************************** 
%************************************************************************** 
%Solve Kepler Eq. with Initial value Ek0=Mk
%Use Fzero Function to solve Kepler Eq.
%The fzero command is an M-file. The algorithm, which was originated by 
%T. Dekker, uses a combination of bisection, secant, and inverse quadratic
%interpolation methods. An Algol 60 version, with some improvements, is 
%given in [Brent, R., Algorithms for Minimization Without Derivatives, 
%Prentice-Hall, 1973.]
%Input:
    %Mk(semicircle): Mean Anomaly
    %e:  Eccentricity
%Output:
    %Ek(Rad):  Eccentric Anomaly
Ek=fzero(@(x) Kepler_Eq(x,Mk*pi,e),Mk*pi);
%**************************************************************************
%************************************************************************** 
%Compute True Anomaly as a Function of Eccentric Anomaly
%Input:
    %Ek(Rad):  Eccentric Anomaly
    %e      :  Eccentricity
%Output:
    %nuk(Rad): True Anomaly
Sin_nuk=sqrt(1-e^2)*sin(Ek)/(1-e*cos(Ek));
Cos_nuk=(cos(Ek)-e)/(1-e*cos(Ek));
nuk=atan2(Sin_nuk,Cos_nuk);
if nuk<0
    nuk=nuk+2*pi;
end
%**************************************************************************
%************************************************************************** 
%Compute Argumet of Lattitude and Correction
%Input:
    %nuk(Rad): True Anomaly
    %omega(semicircle):  Argument of perigee
%Output:
    %Phik(Rad):  Argument of lattitude
Phik=nuk+omega*pi;

%**************************************************************************
%************************************************************************** 
%Compute Argumet of Lattitude,Radious and Inclination Correction
%SEcond Harmonic Perturbation
%Input:
    %Phik(Rad):  Argument of lattitude
    %C_us:Amplitude of the cosine harmonic correction term to the argument
    %     of lattitude(Rad)
    %C_uc:Amplitude of the sine harmonic correction term to the argument
    %     of lattitude(Rad)
    %C_rs:Amplitude of the cosine harmonic correction term to orbit
    %radius(meter)
    %C_rc:Amplitude of the sine harmonic correction term to orbit
    %radius(meter)
    %C_is:Amplitude of the cosine harmonic correction term to the angle of
    %     inclination(Rad)
    %C_ic:Amplitude of the sine harmonic correction term to the angle of
    %     inclination(Rad)
%Output:
    % Delta_uk(Rad):Argument of lattitude correction
    % Delta_rk(meter):Argument of radius correction
    % Delta_ik(Rad):Argument of inclination correction

Delta_uk=C_us*sin(2*Phik)+C_uc*cos(2*Phik);
Delta_rk=C_rs*sin(2*Phik)+C_rc*cos(2*Phik);
Delta_ik=C_is*sin(2*Phik)+C_ic*cos(2*Phik);
%**************************************************************************
%************************************************************************** 
%Compute Corrected Value of Lattitude,Radious,Inclination and Ascendong
%node
%Input:
    % Phik(Rad):  Argument of lattitude
    % A(meter): Semi-major axis
    % Ek(Rad):  Eccentric Anomaly
    % e:  Eccentricity    
    % i0(semi-circle): inlination angle at reference time
    % Omega0(semi-circle): Refernce Longitude of Ascending Node
    % Omegadot(semi-circle/sec): rate of right ascension
    % Omega_dote(rad/sec):WGS 84 value of the earth's rotation rate
    % IDOT(semi-circle/sec): rate of inlination angle
    % Delta_uk(Rad):Argument of lattitude correction
    % Delta_rk(meter):Argument of radius correction
    % Delta_ik(Rad):Argument of inclination correction
%Output:
    % uk(Rad):Corrcted argument of lattitude
    % rk(meter): corrected radius
    % ik(Rad):corrected inclination
    % Omegak(Rad):Corrected longitude of ascending node.
uk=Phik+Delta_uk;              %Latitude
rk=A*(1-e*cos(Ek))+Delta_rk;   %Radious
ik=i0*pi+Delta_ik+IDOT*tk*pi;  %Inclination
Omegak=Omega0*pi+(Omegadot*pi-Omega_dote)*tk-Omega_dote*toe;

%**************************************************************************
%************************************************************************** 
%Compute satellite vehicle position
%Satellite position in orbital plane
x_Perk=rk*cos(uk);
y_Perk=rk*sin(uk);
%Satellite Position in ECEF
xk=x_Perk*cos(Omegak)-y_Perk*cos(ik)*sin(Omegak);
yk=x_Perk*sin(Omegak)+y_Perk*cos(ik)*cos(Omegak);
zk=y_Perk*sin(ik);
%Output:
    % xk,yk,zk(meter): Satellite Position in ECEF
    % Mk(semicircle): Mean Anomaly
    % Ek(Rad):  Eccentric Anomaly
    % nuk(Rad): True Anomaly
    % Phik(Rad):  Argument of lattitude
    % uk(Rad):Corrcted argument of lattitude
    % rk(meter): corrected radius
    % ik(Rad):corrected inclination
    % Omegak(Rad):Corrected longitude of ascending node.
    % A(meter):semi-major orbit axis
    % e: orbit eccentricity

Pos_xyz=[xk yk zk Mk Ek nuk Phik uk rk ik Omegak A e];

Pos_xyz_Mat(ST_No,1)=Pos_xyz(1);Pos_xyz_Mat(ST_No,2)=Pos_xyz(2);Pos_xyz_Mat(ST_No,3)=Pos_xyz(3);
for j=1:10
Orbit_parameter(j,ST_No)=Pos_xyz(j+3);
end
end


