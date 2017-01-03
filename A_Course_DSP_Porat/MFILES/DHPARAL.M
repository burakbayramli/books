function [dH,sc] = dhparal(nsec,dsec,c,K,theta);
% Synopsis: [dH,sc] = dhparal(nsec,dsec,c,K,theta).
% Computes the derivatives of the magnitude response of an
% IIR filter in parallel realization with respect to the
% parameters, and a scaling vector for the parameters.
% Input parameters:
% nsec, dsec, c: parameters of the parallel realization
% K: number of frequency points
% theta: frequency interval (2-element vector).
% Output parameters:
% dH: matrix of partial derivatives of |H(theta)|
% sc: a scaling vector.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

dHn = []; dHd = []; scn = []; scd = [];
[M,junk] = size(nsec);
for k = 1:M,
   if (dsec(k,3) == 0),
      [dHt,sct] = dhdirect(nsec(k,1),dsec(k,1:2),K,theta);
      dHn = [dHn; dHt(1,:)];  dHd = [dHd; dHt(2,:)]; 
      scn = [scn; sct(1)]; scd = [scd; sct(2)];
   else,
      [dHt,sct] = dhdirect(nsec(k,:),dsec(k,:),K,theta);
      dHn = [dHn; dHt(1:2,:)]; dHd = [dHd; dHt(3:4,:)]; 
      scn = [scn; sct(1)*ones(2,1)];
      scd = [scd; sct(2)*ones(2,1)];
   end
end
dH = [dHn; dHd; ones(1,K)]; sc = [scn; scd; scale2(c)];

