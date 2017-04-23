%This Function calculate Satellite Clock Error Base on 
%application . edited by B. Parkinson,J. Spilker, P.Enge, AIAA,1996
%CopyRight By Moein Mehrtash
%**************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/23/2008              *
% Email: moeinmehrtash@yahoo.com                                          *
%**************************************************************************
% Reference:"GPS Theory and application",edited by B.Parkinson,J.Spilker, *
%**************************************************************************
%Relativistic Error
%dTclk_Rel=F*ec*sqrt(A)*sin(E)-Tgd  (Sec)
%F :               =>Rel correction constant
%ec:(Meter)        =>Ecentricity 
%A:(Meter)         =>Orbit Semi major axis  
%E:(Rad)           =>Orbit Eccentric Anomaly 
%Tgd:(Sec)         =>Group Delay
%**************************************************************************
function dTclk_Rel=Error_Satellite_Clock_Relavastic(F,ec,A,E,Tgd);
No_SV=length(A);
for i=1:No_SV
    dTclk_Rel(i)=F*ec(i)*sqrt(A(i))*sin(E(i))-Tgd(i);
end

