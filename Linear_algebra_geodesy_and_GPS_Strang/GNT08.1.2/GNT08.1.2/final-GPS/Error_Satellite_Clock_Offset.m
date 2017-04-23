%This Function calculate Satellite Offest Clock Error Base on GPS Theory  * 
%and application . edited by B. Parkinson,J. Spilker, P.Enge, AIAA,1996   *
%CopyRight By Moein Mehrtash
%**************************************************************************
% Written by Moein Mehrtash, Concordia University, 3/23/2008              *
% Email: moeinmehrtash@yahoo.com                                          *
%**************************************************************************
% Reference:"GPS Theory and application",edited by B.Parkinson,J.Spilker, *
%**************************************************************************
%Ofset Model:                                                             *
%dTclk_Ofset=af0+af1*(T-Toc)+af2*(T-Toc)^2+.....                          *
%af :(1/Sec^i)    =>Matrix of Coeeficient for satellite offset            *
%Ttr:(Sec)        => Time of transmission                                 *
%Toc:(Sec)        => Sv Clock refernce time                               *
%dTclk_Ofset:(Sec)=> Sv Clock offset time                                 *
%**************************************************************************
function dTclk_Offset=Error_Satellite_Clock_Offset(af,Ttr,Toc);

Dim1=size(af);
Order_Coef=Dim1(2);
No_SV=length(Toc);

for j=1:No_SV
    dTclk_Ofset=0;
    T(j)=Ttr(j)-Toc(j);
    if T(j)>302400
        T(j)=T(j)-604800;
    else if T(j)<-302400
        T(j)=T(j)+604800;
        end
    end
    for i=1:Order_Coef
        dTclk_Ofset=dTclk_Ofset+af(j,i)*T(j)^(i-1);
    end
    dTclk_Offset(j)=dTclk_Ofset;
end
    