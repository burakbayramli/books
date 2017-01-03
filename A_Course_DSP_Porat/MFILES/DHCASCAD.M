function [dH,sc] = dhcascad(nsec,dsec,c,K,theta);
% Synopsis: [dH,sc] = cascad(nsec,dsec,c,K,theta).
% Computes the derivatives of the magnitude response of an
% IIR filter in cascade realization with respect to the
% parameters, and a scaling vector for the parameters.
% Input parameters:
% nsec, dsec, c: parameters of the cascade realization
% K: number of frequency points
% theta: frequency interval (2-element vector).
% Output parameters:
% dH: matrix of partial derivatives of |H(theta)|
% sc: a scaling vector.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

dHn = []; dHd = []; scn = []; scd = [];
cntd = 0; cntn = 0;
[M,junk] = size(nsec); H = ones(1,K);
for k = 1:M,
   if (nsec(k,3) ~= 0 & abs(nsec(k,2)) ~= 2),
      Ht = frqresp(nsec(k,:),dsec(k,:),K,theta);
      [dHt,sct] = dhdirect(nsec(k,:),dsec(k,:),K,theta);
      H = Ht.*H;
      dHn = [dHn; dHt(2,:)./Ht]; cntn = cntn+1;
      dHd = [dHd; dHt(4:5,:)./(ones(2,1)*Ht)]; 
      cntd = cntd+2;
      scn = [scn; sct(2,1)];
      scd = [scd; sct(4:5,1)];
   end
end
dHn = c*(ones(cntn,1)*H).*dHn; dHd = c*(ones(cntd,1)*H).*dHd;
dH = [dHn; dHd; H]; sc = [scn; scd; scale2(c)];

