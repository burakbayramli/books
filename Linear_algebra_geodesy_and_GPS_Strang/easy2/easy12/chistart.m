function Chi2 = chistart (D,L,a,ncands,factor)
%CHISTART: Computes the initial size of the search ellipsoid
%
% This routine computes or approximates the initial size of the
% search ellipsoid. If the requested number of candidates is not
% more than the dimension + 1, this is done by computing the squared
% distances of partially rounded float vectors to the float vector in
% the metric of the covariance matrix. Otherwise an approximation is used.
%
% Input arguments
%    L,D   : LtDL-decomposition of the variance-covariance matrix of
%            the float ambiguities (preferably decorrelated)
%    a     : float ambiguites (preferably decorrelated)
%    ncands: Requested number of candidates (default = 2)
%    factor: Multiplication factor for the volume of the resulting
%            search ellipsoid (default = 1.5)
%
% Output arguments:
%    Chi2  : Size of the search ellipsoid

% ----------------------------------------------------------------------
% File.....: chistart.m
% Date.....: 19-MAY-1999
% Author...: Peter Joosten
%            Mathematical Geodesy and Positioning
%            Delft University of Technology
% ----------------------------------------------------------------------

% ------------------
% --- Initialize ---
% ------------------

if nargin < 4; ncands = 2  ; end;
if nargin < 5; factor = 1.5; end;

n = max(size(a));

% ----------------------------------------------------------------------
% --- Computation depends on the number of candidates to be computed ---
% ----------------------------------------------------------------------

if ncands == 1;
  
  % ---------------------------------------------------
  % --- The squared norm, based on the bootstrapped ---
  % --- solution will be computed                   ---
  % ---------------------------------------------------

  afloat = a;
  afixed = a;

  for i = 1:n;

    dw = 0;
    for j = 1:n-1;
      dw = dw + L(i,j) * (a(j) - afixed(j));
    end;

    a(i) = a(i) - dw;
    afixed(i) = round (a(i));
   
  end;

  Chi2 = (afloat-afixed)' * inv(L'*diag(D)*L) * (afloat-afixed) + 1d-6;
  
elseif ncands <= n+1;

  % ----------------------------------------------
  % --- The right squared norm can be computed ---
  % ----------------------------------------------
   
   Linv = inv(L);
   Dinv = 1./D;
   
   dist = round(a) - a;
   e    = Linv'*dist;
   Chi  = [zeros(1,n) sum(Dinv' .* e .* e)];

   % ------------------------------------------
   % --- Compute the real squared distances ---
   % ------------------------------------------

   for i = 1:n;

      Chi(i) = 0;
      for j = 1:i; 
         Chi(i) = Chi(i) + Dinv(j) * Linv(i,j) * (2*e(j)+Linv(i,j));
      end;
      Chi(i) = Chi(n+1) + abs(Chi(i));
   
   end;

   % ---------------------------------------------------------------
   % --- Sort the results, and return the appropriate number     ---
   % --- Add an "eps", to make sure there is no boundary problem ---
   % ---------------------------------------------------------------

   Chi  = sort(Chi);
   Chi2 = Chi(ncands) + 1d-6;
   
else

   % -----------------------------------------------------
   % An approximation for the squared norm is computed ---
   % -----------------------------------------------------

   Linv = inv(L);
   Dinv = 1./D;
   
   Vn   = (2/n) * (pi ^ (n/2) / gamma(n/2));
   Chi2 = factor * (ncands / sqrt((prod(1 ./ Dinv)) * Vn)) ^ (2/n);

end;

% ----------------------------------------------------------------------
% End of routine: chistart
% ----------------------------------------------------------------------
