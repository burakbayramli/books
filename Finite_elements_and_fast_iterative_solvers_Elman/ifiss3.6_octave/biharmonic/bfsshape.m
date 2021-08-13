      function [psi,dpsids,dpsidt,dpsi2ds2,dpsi2dt2,dpsi2dsdt] = bfsshape(s,t)
%BFSSHAPE evaluates bicubic shape functions on reference element
%   [psi,dpsids,dpsidt,dpsi2ds2,dpsi2dt2,dpsi2dsdt] = bfsshape(s,t);
%   input
%          s         x coordinate   
%          t         y coordinate
%   output
%          psi         shape function
%          dpsids      s derivative of psi
%          dpsidt      t derivative of psi
%          dpsi2ds2    s second derivatives of psi
%          dpsi2dt2    t second derivatives of psi
%          dpsi2dsdt   x second derivatives of psi
%
%   IFISS function: DJS; 13 August 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
%
% one dimensional shape functions
phix1= 0.5*(1-s);  phix2= 0.5*(s+1);
phiy1= 0.5*(1-t);  phiy2= 0.5*(t+1);
%
ellx(2)= phix2.*phix2.*(1+2*phix1); ellx(1)= 1-ellx(2);
elly(2)= phiy2.*phiy2.*(1+2*phiy1); elly(1)= 1-elly(2);
ellx(3)= 2*phix1.*phix1.*phix2; ellx(4)= -2*phix1.*phix2.*phix2;
elly(3)= 2*phiy1.*phiy1.*phiy2; elly(4)= -2*phiy1.*phiy2.*phiy2;
%
dellx(2)= 3*phix2.*phix1; dellx(1)= -dellx(2);
delly(2)= 3*phiy2.*phiy1; delly(1)= -delly(2);
dellx(3)= phix1.*(1-3*phix2);  delly(3)= phiy1.*(1-3*phiy2);
dellx(4)= phix2.*(1-3*phix1);  delly(4)= phiy2.*(1-3*phiy1);
%
ddellx(1)= 1.5*s; ddellx(2)= -ddellx(1);
ddelly(1)= 1.5*t; ddelly(2)= -ddelly(1);
ddellx(3)= 0.5*(3*s-1); ddellx(4)= 0.5*(3*s+1);
ddelly(3)= 0.5*(3*t-1); ddelly(4)= 0.5*(3*t+1);
%
% two dimensional shape functions
	  psi(1) = ellx(1)*elly(1);
	  psi(2) = ellx(2)*elly(1);
	  psi(3) = ellx(2)*elly(2);
      psi(4) = ellx(1)*elly(2);
	  psi(5) = ellx(3)*elly(1);
	  psi(6) = ellx(4)*elly(1);
	  psi(7) = ellx(4)*elly(2);
	  psi(8) = ellx(3)*elly(2);
      psi(9) = ellx(1)*elly(3);
     psi(10) = ellx(2)*elly(3);
     psi(11) = ellx(2)*elly(4);
     psi(12) = ellx(1)*elly(4);
     psi(13) = ellx(3)*elly(3);
     psi(14) = ellx(4)*elly(3);
     psi(15) = ellx(4)*elly(4);
     psi(16) = ellx(3)*elly(4);
%
% first derivatives
   dpsids(1) = dellx(1)*elly(1);
   dpsids(2) = dellx(2)*elly(1);
   dpsids(3) = dellx(2)*elly(2);
   dpsids(4) = dellx(1)*elly(2);
   dpsids(5) = dellx(3)*elly(1);
   dpsids(6) = dellx(4)*elly(1);
   dpsids(7) = dellx(4)*elly(2);
   dpsids(8) = dellx(3)*elly(2);
   dpsids(9) = dellx(1)*elly(3);
   dpsids(10) = dellx(2)*elly(3);
   dpsids(11) = dellx(2)*elly(4);
   dpsids(12) = dellx(1)*elly(4);
   dpsids(13) = dellx(3)*elly(3);
   dpsids(14) = dellx(4)*elly(3);
   dpsids(15) = dellx(4)*elly(4);
   dpsids(16) = dellx(3)*elly(4);
   dpsidt(1) = ellx(1)*delly(1);
   dpsidt(2) = ellx(2)*delly(1);
   dpsidt(3) = ellx(2)*delly(2);
   dpsidt(4) = ellx(1)*delly(2);
   dpsidt(5) = ellx(3)*delly(1);
   dpsidt(6) = ellx(4)*delly(1);
   dpsidt(7) = ellx(4)*delly(2);
   dpsidt(8) = ellx(3)*delly(2);
   dpsidt(9) = ellx(1)*delly(3);
   dpsidt(10) = ellx(2)*delly(3);
   dpsidt(11) = ellx(2)*delly(4);
   dpsidt(12) = ellx(1)*delly(4);
   dpsidt(13) = ellx(3)*delly(3);
   dpsidt(14) = ellx(4)*delly(3);
   dpsidt(15) = ellx(4)*delly(4);
   dpsidt(16) = ellx(3)*delly(4);
%
% second derivatives
   dpsi2ds2(1) = ddellx(1)*elly(1);
   dpsi2ds2(2) = ddellx(2)*elly(1);
   dpsi2ds2(3) = ddellx(2)*elly(2);
   dpsi2ds2(4) = ddellx(1)*elly(2);
   dpsi2ds2(5) = ddellx(3)*elly(1);
   dpsi2ds2(6) = ddellx(4)*elly(1);
   dpsi2ds2(7) = ddellx(4)*elly(2);
   dpsi2ds2(8) = ddellx(3)*elly(2);
   dpsi2ds2(9) = ddellx(1)*elly(3);
   dpsi2ds2(10) = ddellx(2)*elly(3);
   dpsi2ds2(11) = ddellx(2)*elly(4);
   dpsi2ds2(12) = ddellx(1)*elly(4);
   dpsi2ds2(13) = ddellx(3)*elly(3);
   dpsi2ds2(14) = ddellx(4)*elly(3);
   dpsi2ds2(15) = ddellx(4)*elly(4);
   dpsi2ds2(16) = ddellx(3)*elly(4);
   dpsi2dt2(1) = ellx(1)*ddelly(1);
   dpsi2dt2(2) = ellx(2)*ddelly(1);
   dpsi2dt2(3) = ellx(2)*ddelly(2);
   dpsi2dt2(4) = ellx(1)*ddelly(2);
   dpsi2dt2(5) = ellx(3)*ddelly(1);
   dpsi2dt2(6) = ellx(4)*ddelly(1);
   dpsi2dt2(7) = ellx(4)*ddelly(2);
   dpsi2dt2(8) = ellx(3)*ddelly(2);
   dpsi2dt2(9) = ellx(1)*ddelly(3);
   dpsi2dt2(10) = ellx(2)*ddelly(3);
   dpsi2dt2(11) = ellx(2)*ddelly(4);
   dpsi2dt2(12) = ellx(1)*ddelly(4);
   dpsi2dt2(13) = ellx(3)*ddelly(3);
   dpsi2dt2(14) = ellx(4)*ddelly(3);
   dpsi2dt2(15) = ellx(4)*ddelly(4);
   dpsi2dt2(16) = ellx(3)*ddelly(4);
%
%  cross derivatives
dpsi2dsdt(1) = dellx(1)*delly(1);
dpsi2dsdt(2) = dellx(2)*delly(1);
dpsi2dsdt(3) = dellx(2)*delly(2);
dpsi2dsdt(4) = dellx(1)*delly(2);
dpsi2dsdt(5) = dellx(3)*delly(1);
dpsi2dsdt(6) = dellx(4)*delly(1);
dpsi2dsdt(7) = dellx(4)*delly(2);
dpsi2dsdt(8) = dellx(3)*delly(2);
dpsi2dsdt(9) = dellx(1)*delly(3);
dpsi2dsdt(10) = dellx(2)*delly(3);
dpsi2dsdt(11) = dellx(2)*delly(4);
dpsi2dsdt(12) = dellx(1)*delly(4);
dpsi2dsdt(13) = dellx(3)*delly(3);
dpsi2dsdt(14) = dellx(4)*delly(3);
dpsi2dsdt(15) = dellx(4)*delly(4);
dpsi2dsdt(16) = dellx(3)*delly(4);

%------ debug output
%fprintf('\n\n new quadrature point .. ')
%fprintf('\n s = %9.6f | t = %9.6f \n',s,t)
%fprintf('\n xx, yy and xy derivatives \n')
%disp([dpsi2ds2',dpsi2dt2',dpsi2dsdt']),
%------ end debug output
       
      return
