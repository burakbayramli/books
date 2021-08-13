 function [qua,dquads,dquadt] = qqshape(s,t)
%QQSHAPE evaluates biquartic shape functions 
%   [quar,dquards,dquardt] = qqshape(s,t);
%   input
%          s         x coordinate   
%          t         y coordinate
%   output
%          qua        shape function
%          dquads     x derivative of cpsi
%          dquadt     y derivative of cpsi
%
%   IFISS function: DJS; 3 January 2011.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao 

%% Q4 ELement nodes        

%                 4     15    7     14    3 
%                 
%                 16    24    23    22    13
%                 
%                 8     25    9     21    6                                  
%                                                  
%                 17    18    19    20    12
%                                                                 
%                 1     10    5     11    2

%%

% one dimensional shape functions      
        ellx(1)=2/3*(s+1/2)*s*(s-1/2)*(s-1);      elly(1)=2/3*(t+1/2)*t*(t-1/2)*(t-1);      
        ellx(2)=-8/3*(s+1)*s*(s-1/2)*(s-1);       elly(2)=-8/3*(t+1)*t*(t-1/2)*(t-1);
        ellx(3)=4*(s+1)*(s+1/2)*(s-1/2)*(s-1);    elly(3)=4*(t+1)*(t+1/2)*(t-1/2)*(t-1);
        ellx(4)=-8/3*(s+1)*(s+1/2)*s*(s-1);       elly(4)=-8/3*(t+1)*(t+1/2)*t*(t-1);
        ellx(5)=2/3*(s+1)*(s+1/2)*s*(s-1/2);      elly(5)=2/3*(t+1)*(t+1/2)*t*(t-1/2);
     
        dellx(1)=8/3*s^3-2*s^2-1/3*s+1/6;        delly(1)=8/3*t^3-2*t^2-1/3*t+1/6;
        dellx(2)=-32/3*s^3+4*s^2+16/3*s-4/3;     delly(2)=-32/3*t^3+4*t^2+16/3*t-4/3;
        dellx(3)=16*s^3-10*s;                    delly(3)=16*t^3-10*t;
        dellx(4)=-32/3*s^3-4*s^2+16/3*s+4/3;     delly(4)=-32/3*t^3-4*t^2+16/3*t+4/3;
        dellx(5)=8/3*s^3-1/3*s+2*s^2-1/6;        delly(5)=8/3*t^3-1/3*t+2*t^2-1/6;
 
       
% two dimensional shape functions	 
      qua(1) = ellx(1)*elly(1);
      qua(2) = ellx(5)*elly(1);
      qua(3) = ellx(5)*elly(5);
      qua(4) = ellx(1)*elly(5);
      qua(5) = ellx(3)*elly(1);
      qua(6) = ellx(5)*elly(3);
      qua(7) = ellx(3)*elly(5);
      qua(8) = ellx(1)*elly(3);
      qua(9) = ellx(3)*elly(3);
      qua(10)= ellx(2)*elly(1);
      qua(11)= ellx(4)*elly(1);
      qua(12)= ellx(5)*elly(2);
      qua(13)= ellx(5)*elly(4);
      qua(14)= ellx(4)*elly(5);
      qua(15)= ellx(2)*elly(5);
      qua(16)= ellx(1)*elly(4);
      qua(17)= ellx(1)*elly(2);
      qua(18)= ellx(2)*elly(2);
      qua(19)= ellx(3)*elly(2);
      qua(20)= ellx(4)*elly(2);
      qua(21)= ellx(4)*elly(3);
      qua(22)= ellx(4)*elly(4);
      qua(23)= ellx(3)*elly(4);
      qua(24)= ellx(2)*elly(4);
      qua(25)= ellx(2)*elly(3);
      
      dquads(1) = dellx(1)*elly(1);      
      dquads(2) = dellx(5)*elly(1);
      dquads(3) = dellx(5)*elly(5);
      dquads(4) = dellx(1)*elly(5);
      dquads(5) = dellx(3)*elly(1);
      dquads(6) = dellx(5)*elly(3);
      dquads(7) = dellx(3)*elly(5);
      dquads(8) = dellx(1)*elly(3);
      dquads(9) = dellx(3)*elly(3);
      dquads(10)= dellx(2)*elly(1);
      dquads(11)= dellx(4)*elly(1);
      dquads(12)= dellx(5)*elly(2);
      dquads(13)= dellx(5)*elly(4);
      dquads(14)= dellx(4)*elly(5);
      dquads(15)= dellx(2)*elly(5);
      dquads(16)= dellx(1)*elly(4);
      dquads(17)= dellx(1)*elly(2);
      dquads(18)= dellx(2)*elly(2);
      dquads(19)= dellx(3)*elly(2);
      dquads(20)= dellx(4)*elly(2);
      dquads(21)= dellx(4)*elly(3);
      dquads(22)= dellx(4)*elly(4);
      dquads(23)= dellx(3)*elly(4);
      dquads(24)= dellx(2)*elly(4);
      dquads(25)= dellx(2)*elly(3);
 
      dquadt(1) = ellx(1)*delly(1);
      dquadt(2) = ellx(5)*delly(1);
      dquadt(3) = ellx(5)*delly(5);
      dquadt(4) = ellx(1)*delly(5);
      dquadt(5) = ellx(3)*delly(1);
      dquadt(6) = ellx(5)*delly(3);
      dquadt(7) = ellx(3)*delly(5);
      dquadt(8) = ellx(1)*delly(3);
      dquadt(9) = ellx(3)*delly(3);
      dquadt(10)= ellx(2)*delly(1);
      dquadt(11)= ellx(4)*delly(1);
      dquadt(12)= ellx(5)*delly(2);
      dquadt(13)= ellx(5)*delly(4);
      dquadt(14)= ellx(4)*delly(5);
      dquadt(15)= ellx(2)*delly(5);
      dquadt(16)= ellx(1)*delly(4);
      dquadt(17)= ellx(1)*delly(2);
      dquadt(18)= ellx(2)*delly(2);
      dquadt(19)= ellx(3)*delly(2);
      dquadt(20)= ellx(4)*delly(2);
      dquadt(21)= ellx(4)*delly(3);
      dquadt(22)= ellx(4)*delly(4);
      dquadt(23)= ellx(3)*delly(4);
      dquadt(24)= ellx(2)*delly(4);
      dquadt(25)= ellx(2)*delly(3);
    
      
      return
