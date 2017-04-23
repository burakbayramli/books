%This Function use Ephemeris Data and Calculate satellite Position 
%CopyRight By Moein Mehrtash
%**************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/28/2008              *
% Email: moeinmehrtash@yahoo.com                                          *
%**************************************************************************
%**************************************************************************
% Satellite Position By Ephemeris Model 
%Function's Inputs:
    %Pos_SV(m):Satellite Position Matrix
    %Pos_Rcv(m):GPS reciever Position
    %Rho(m):Pseudo Range

    
%Function's Outputs:
    %G:
    %Delta_X:
    %Pos_RCV_N:
    %B:
    






%**************************************************************************
%**************************************************************************

function [G,Delta_X,Pos_Rcv_n,B]=Gen_G_DX_XYZ_B(Pos_SV,Pos_Rcv,Rho);
[m,n]=size(Pos_SV);
d=Distance(Pos_SV,Pos_Rcv);

for i=1:m
    dif=Pos_SV(i,:)-Pos_Rcv;
    unit=dif./d(i);
    for j=1:n
        Unit_Mtrix(i,j)=unit(j);
    end
end
G=[-Unit_Mtrix ones(m,1)];
Delta_Rho=(Rho-d');    
Delta_X=inv(G'*G)*G'*Delta_Rho;
Pos_Rcv_n=(Pos_Rcv'+Delta_X(1:3))';
B=Delta_X(4);






