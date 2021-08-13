function [dpsi2ds2,dpsi2dt2] = qshape_2(s,t)
%QSHAPE_2 evaluates biquadratic second derivatives 
% [dpsi2ds2,dpsi2dt2] = qshape_2(s,t);  
%   input
%          s         x coordinate   
%          t         y coordinate
%   output
%          dpsi2ds2   x second derivative of psi
%          dpsi2dt2   y second derivative of psi
%
%   IFISS function: QL; 1 Jun 2009.
% Copyright (c) 2010 D.J. Silvester, Qifeng Liao
      one = 1.0e0;
%
% one dimensional shape functions      
      ellx(1) = 0.5*s*(s-1);  elly(1) = 0.5*t*(t-1);
      ellx(2) = 1-(s*s);      elly(2) = 1-(t*t);
	  ellx(3) = 0.5*s*(s+1);  elly(3) = 0.5*t*(t+1);
%     dellx(1) = s-0.5;       delly(1) = t-0.5;
% 	  dellx(2) = -2*s;        delly(2) = -2*t;
%	  dellx(3) = s+0.5;       delly(3) = t+0.5;
      ddellx(1)=1;            ddelly(1)=1;
      ddellx(2)=-2;           ddelly(2)=-2;
      ddellx(3)=1;            ddelly(3)=1;
% two dimensional shape functions	  
%	  psi(1) = ellx(1)*elly(1);
%	  psi(2) = ellx(3)*elly(1);
%	  psi(3) = ellx(3)*elly(3);
%	  psi(4) = ellx(1)*elly(3);
%	  psi(5) = ellx(2)*elly(1);
%	  psi(6) = ellx(3)*elly(2);
%	  psi(7) = ellx(2)*elly(3);
%	  psi(8) = ellx(1)*elly(2);
%	  psi(9) = ellx(2)*elly(2);
      
      dpsi2ds2(1)=ddellx(1)*elly(1);
      dpsi2ds2(2)=ddellx(3)*elly(1);
      dpsi2ds2(3)=ddellx(3)*elly(3);
      dpsi2ds2(4)=ddellx(1)*elly(3);
      dpsi2ds2(5)=ddellx(2)*elly(1);
      dpsi2ds2(6)=ddellx(3)*elly(2);
      dpsi2ds2(7)=ddellx(2)*elly(3);
      dpsi2ds2(8)=ddellx(1)*elly(2);
      dpsi2ds2(9)=ddellx(2)*elly(2);      
      dpsi2dt2(1)=ellx(1)*ddelly(1);
      dpsi2dt2(2)=ellx(3)*ddelly(1);
      dpsi2dt2(3)=ellx(3)*ddelly(3);
      dpsi2dt2(4)=ellx(1)*ddelly(3);
      dpsi2dt2(5)=ellx(2)*ddelly(1);
      dpsi2dt2(6)=ellx(3)*ddelly(2);
      dpsi2dt2(7)=ellx(2)*ddelly(3);
      dpsi2dt2(8)=ellx(1)*ddelly(2);
      dpsi2dt2(9)=ellx(2)*ddelly(2);    
      return
