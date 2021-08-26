function [um,up] = ENO(xloc,uloc,m,Crec);
% Purpose: Reconstruct the left (mu) and right (up) cell interface values 
%          using an ENO reconstruction based on 2m-1 long vectors.

% Treat special case of m=1 - no stencil to select
if (m==1)
    um = uloc(1); up = uloc(1);
else
% Apply ENO procedure to build stencil
   S = zeros(m,1); S(1) = m;
   for mm=1:m-1
      % Left stencil
      Sindxl = [S(1)-1; S(1:mm)];
      [Vl,DDNmat] = ddnewton(xloc(Sindxl),uloc(Sindxl));
  
      % Right stencil 
      Sindxr = [S(1:mm); S(mm)+1];
      [Vr,DDNmat] = ddnewton(xloc(Sindxr),uloc(Sindxr));

      % Choose stencil by divided differences
      S(1:mm+1) = Sindxl;
      if (Vl>Vr) S(1:mm+1) = Sindxr; end;   
   end

   % Compute stencil shift 'r' and cell interface values
   r = m - S(1);
   up = Crec(r+2,:)*uloc(S); um = Crec(r+1,:)*uloc(S);
end
return