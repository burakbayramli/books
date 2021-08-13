 function [cpsi,dcpsids,dcpsidt] = cshape(s,t)
%CSHAPE evaluates bicubic shape functions 
%   [psi,dpsids,dpsidt] = cshape(s,t);
%   input
%          s         x coordinate   
%          t         y coordinate
%   output
%          cpsi        shape function
%          dcpsids     x derivative of cpsi
%          dcpsidt     y derivative of cpsi
%
%   IFISS function: QL; 17 Jun 2009.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao
%      one = 1.0e0;
%
% one dimensional shape functions      
       ellx(1)=-9/16*(s+1/3)*(s-1/3)*(s-1);   elly(1)=-9/16*(t+1/3)*(t-1/3)*(t-1);
       ellx(2)=27/16*(s+1)*(s-1/3)*(s-1);     elly(2)=27/16*(t+1)*(t-1/3)*(t-1);
       ellx(3)=-27/16*(s+1)*(s+1/3)*(s-1);    elly(3)=-27/16*(t+1)*(t+1/3)*(t-1);
       ellx(4)=9/16*(s+1)*(s+1/3)*(s-1/3);    elly(4)=9/16*(t+1)*(t+1/3)*(t-1/3);
       
       dellx(1)=-9/16*(3*s^2-2*s-1/9);        delly(1)=-9/16*(3*t^2-2*t-1/9);
       dellx(2)=27/16*(3*s^2-2/3*s-1);        delly(2)=27/16*(3*t^2-2/3*t-1);
       dellx(3)=-27/16*(3*s^2+2/3*s-1);       delly(3)=-27/16*(3*t^2+2/3*t-1);
       dellx(4)=9/16*(3*s^2+2*s-1/9);         delly(4)=9/16*(3*t^2+2*t-1/9);
       
% two dimensional shape functions	 
      cpsi(1) = ellx(1)*elly(1);
      cpsi(2) = ellx(4)*elly(1);
      cpsi(3) = ellx(4)*elly(4);
      cpsi(4) = ellx(1)*elly(4);
      cpsi(5) = ellx(2)*elly(1);
      cpsi(6) = ellx(3)*elly(1);
      cpsi(7) = ellx(4)*elly(2);
      cpsi(8) = ellx(4)*elly(3);
      cpsi(9) = ellx(3)*elly(4);
      cpsi(10)= ellx(2)*elly(4);
      cpsi(11)= ellx(1)*elly(3);
      cpsi(12)= ellx(1)*elly(2);
      cpsi(13)= ellx(2)*elly(2);
      cpsi(14)= ellx(3)*elly(2);
      cpsi(15)= ellx(3)*elly(3);
      cpsi(16)= ellx(2)*elly(3);
      
      dcpsids(1) = dellx(1)*elly(1);
      dcpsids(2) = dellx(4)*elly(1);
      dcpsids(3) = dellx(4)*elly(4);
      dcpsids(4) = dellx(1)*elly(4);
      dcpsids(5) = dellx(2)*elly(1);
      dcpsids(6) = dellx(3)*elly(1);
      dcpsids(7) = dellx(4)*elly(2);
      dcpsids(8) = dellx(4)*elly(3);
      dcpsids(9) = dellx(3)*elly(4);
      dcpsids(10)= dellx(2)*elly(4);
      dcpsids(11)= dellx(1)*elly(3);
      dcpsids(12)= dellx(1)*elly(2);
      dcpsids(13)= dellx(2)*elly(2);
      dcpsids(14)= dellx(3)*elly(2);
      dcpsids(15)= dellx(3)*elly(3);
      dcpsids(16)= dellx(2)*elly(3);
      
      dcpsidt(1) = ellx(1)*delly(1);
      dcpsidt(2) = ellx(4)*delly(1);
      dcpsidt(3) = ellx(4)*delly(4);
      dcpsidt(4) = ellx(1)*delly(4);
      dcpsidt(5) = ellx(2)*delly(1);
      dcpsidt(6) = ellx(3)*delly(1);
      dcpsidt(7) = ellx(4)*delly(2);
      dcpsidt(8) = ellx(4)*delly(3);
      dcpsidt(9) = ellx(3)*delly(4);
      dcpsidt(10)= ellx(2)*delly(4);
      dcpsidt(11)= ellx(1)*delly(3);
      dcpsidt(12)= ellx(1)*delly(2);
      dcpsidt(13)= ellx(2)*delly(2);
      dcpsidt(14)= ellx(3)*delly(2);
      dcpsidt(15)= ellx(3)*delly(3);
      dcpsidt(16)= ellx(2)*delly(3);
      
      return
