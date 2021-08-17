function Q = IsentropicVortexBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
  
%  function [Q] = IsentropicVortexBC2D(xin, yin, nxin, nyin, mapI, mapO, mapW, mapC, Q, time);
% Purpose: Impose boundary conditions on 2D Euler equations on weak form

Qbc = IsentropicVortexIC2D(xin, yin, time);

mapB = [mapI;mapO;mapW];

for n=1:4
  Qn = Q(:,:,n);  Qbcn = Qbc(:,:,n); Qn(mapB) = Qbcn(mapB);
  Q(:,:,n) = Qn;
end
return
