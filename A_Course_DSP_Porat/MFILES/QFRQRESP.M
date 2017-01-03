function H = qfrqresp(typ,B,b,a,K,theta);
% Synopsis: H = qfrqresp(typ,B,b,a,K,theta).
% Computes the frequency response of a filter subject
% to coefficient quantization.
% Input parameters:
% typ: 'd' for direct, 'p' for parallel, 'c' for cascade
% b, a: numerator and denominator polynomials
% K: number of frequency points
% theta: frequency interval (2-element vector).
% Output parameters:
% H: the frequency response.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

if (typ == 'd'),
   scn = (2^(B-1))/scale2(b); b = (1/scn)*round(scn*b);
   scd = (2^(B-1))/scale2(a); a = (1/scd)*round(scd*a);
   H = frqresp(b,a,K,theta);
elseif (typ == 'p'),
   [c,nsec,dsec] = tf2rpf(b,a);
   sc = (2^(B-1))/scale2(c); c = (1/sc)*round(sc*c);
   [M,junk] = size(nsec); H = c;
   for k = 1:M,
      nt = nsec(k,:); dt = dsec(k,:);
      if (dt(3) == 0), dt = dt(1:2); nt = nt(1); end
      scn = (2^(B-1))/scale2(nt); nt = (1/scn)*round(scn*nt);
      scd = (2^(B-1))/scale2(dt); dt = (1/scd)*round(scd*dt);
      H = H + frqresp(nt,dt,K,theta);
   end
elseif (typ == 'c'),
   c = b(1); v = roots(a); u = roots(b);
   [nsec,dsec] = pairpz(v,u);
   sc = (2^(B-1))/scale2(c); c = (1/sc)*round(sc*c);
   [M,junk] = size(nsec); H = c;
   for k = 1:M,
      nt = nsec(k,:); dt = dsec(k,:);
      if (dt(3) == 0), dt = dt(1:2); nt = nt(1:2); end
      scn = (2^(B-1))/scale2(nt); nt = (1/scn)*round(scn*nt);
      scd = (2^(B-1))/scale2(dt); dt = (1/scd)*round(scd*dt);
      H = H.*frqresp(nt,dt,K,theta);
   end
end

