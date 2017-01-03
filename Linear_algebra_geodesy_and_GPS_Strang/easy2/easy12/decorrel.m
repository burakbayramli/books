function [Q,Z,L,D,z] = decorrel (Q,a);
%DECORREL: Decorrelate a (co)variance matrix of ambiguities
%
% This routine creates a decorrelated Q-matrix, by finding the
% Z-matrix and performing the corresponding transformation.
%
% The method is described in:
% The routine is based on Fortran routines written by Paul de Jonge (TUD)
% and on Matlab-routines written by Kai Borre.
% The resulting Z-matrix can be used as follows:
% z = Zt * a; \hat(z) = Zt * \hat(a);
% Q_\hat(z) = Zt * Q_\hat(a) * Z
%
% Input arguments:
%   a: Original ambiguities
%   Q: Variance/covariance matrix of ambiguities (original)
%
% Output arguments:
%   Q: Decorrelated variance/covariance matrix
%   Z: Z-transformation matrix
%   L: L matrix (from LtDL-decomposition of Q, optional)
%   D: D matrix (from LtDL-decomposition of Q, optional)
%   z: Transformed ambiguities (optional)

% ----------------------------------------------------------------------
% Function.: decorrel
% Date.....: 19-MAY-1999
% Author...: Peter Joosten
%            Mathematical Geodesy and Positioning
%            Delft University of Technology
% ----------------------------------------------------------------------

% -----------------------
% --- Initialisations ---
% -----------------------

n    = size(Q,1);
Zti  = eye(n);
i1   = n - 1;
sw   = 1;

% --------------------------
% --- LtDL decomposition ---
% --------------------------

[L,D] = ldldecom (Q);

% ------------------------------------------
% --- The actual decorrelation procedure ---
% ------------------------------------------


while sw;

   i  = n;
   sw = 0;

   while ( ~sw ) & (i > 1)

      i = i - 1;
      if (i <= i1); 
      
         for j = i+1:n
            mu = round(L(j,i));
            if mu ~= 0
               L(j:n,i) = L(j:n,i) - mu * L(j:n,j);
               Zti(1:n,j) = Zti(1:n,j) + mu * Zti(1:n,i);
            end
        end

      end;
  
      delta = D(i) + L(i+1,i)^2 * D(i+1);
      if (delta < D(i+1))

         lambda(3)    = D(i+1) * L(i+1,i) / delta;
         eta          = D(i) / delta;
         D(i)         = eta * D(i+1);
         D(i+1)       = delta;

         Help         = L(i+1,1:i-1) - L(i+1,i) .* L(i,1:i-1);
         L(i+1,1:i-1) = lambda(3) * L(i+1,1:i-1) + eta * L(i,1:i-1);
         L(i,1:i-1)   = Help;
         L(i+1,i)     = lambda(3);

         Help         = L(i+2:n,i);
         L(i+2:n,i)   = L(i+2:n,i+1);
         L(i+2:n,i+1) = Help;
         
         Help         = Zti(1:n,i);
         Zti(1:n,i)   = Zti(1:n,i+1);
         Zti(1:n,i+1) = Help;

         i1           = i;
         sw           = 1;

      end;

   end;

end;

% ---------------------------------------------------------------------
% --- Return the transformed Q-matrix and the transformation-matrix ---
% --- Return the decorrelated ambiguities, if they were supplied    ---
% ---------------------------------------------------------------------

Z = inv(Zti');
Q = Z' * Q * Z;

if nargin == 2 & nargout >= 5;
  z = Z' * a
end;

% ----------------------------------------------------------------------
% End of routine: decorrel
% ----------------------------------------------------------------------
