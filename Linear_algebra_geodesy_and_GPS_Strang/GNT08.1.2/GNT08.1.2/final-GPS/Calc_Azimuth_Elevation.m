%This Function Compute Azimuth and Elevation of satellite from reciever 
%CopyRight By Moein Mehrtash
%************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/21/2008            *
% Email: moeinmehrtash@yahoo.com                                        *
%************************************************************************           
%    ==================================================================
%    Input :                                                            *
%        Pos_Rcv       : XYZ position of reciever               (Meter) *
%        Pos_SV        : XYZ matrix position of GPS satellites  (Meter) *
%    Output:                                                            *
%        E             :Elevation (Rad)                                 *
%        A             :Azimuth   (Rad)                                 *
%************************************************************************           


function [E,A]=Calc_Azimuth_Elevation(Pos_Rcv,Pos_SV);

R=Pos_SV-Pos_Rcv;               %vector from Reciever to Satellite

GPS = ECEF2GPS(Pos_Rcv);        %Lattitude and Longitude of Reciever
Lat=GPS(1);Lon=GPS(2);

ENU=XYZ2ENU(R,Lat,Lon);
Elevation=asin(ENU(3)/norm(ENU));
Azimuth=atan2(ENU(1)/norm(ENU),ENU(2)/norm(ENU));
E=Elevation;
A=Azimuth;
