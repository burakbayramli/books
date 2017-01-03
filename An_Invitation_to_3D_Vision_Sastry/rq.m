% rq decomposition of matrix A
function [r, q] = rq(a)
 
  
% set A32 to zero
 c = -a(3,3)/sqrt(a(3,2)^2 + a(3,3)^2);
 s = a(3,2)/sqrt(a(3,2)^2 + a(3,3)^2);
 Qx = [1 0 0; 0 c -s; 0 s c];
 
 a1 = a*Qx;

% set A32 to zero
 c = a1(3,3)/sqrt(a1(3,1)^2 + a1(3,3)^2);
 s = a1(3,1)/sqrt(a1(3,1)^2 + a1(3,3)^2);
 Qy = [c 0 s; 0 1 0; -s 0 c];
 
 a2 = a1*Qy;
 
 % to set A21 to zero
 c = -a2(2,2)/sqrt(a2(2,2)^2 + a2(2,1)^2);
 s = a2(2,1)/sqrt(a2(2,2)^2 + a2(2,1)^2);
 Qz = [c -s 0; s c 0; 0 0 1];
 r = a2*Qz;
 q = (Qx*Qy*Qz)';
 % enforce positive values of r
 if (r(1,1) < 0) & r(2,2) < 0 
     r = r*[-1 0 0; 0 -1 0; 0 0 1];
     q = [-1 0 0; 0 -1 0; 0 0 1]*q;
 end;
 if (r(1,1) > 0 &  r(2,2) < 0)
     r = r*[-1 0 0; 0 1 0; 0 0 -1];
     q = [-1 0 0; 0 1 0; 0 0 -1]*q;
 end;
 if (r(1,1) < 0 &  r(2,2) > 0)
     r = r*[-1 0 0; 0 1 0; 0 0 -1];
     q = [-1 0 0; 0 1 0; 0 0 -1]*q;
 end;
 aa = r*q;
