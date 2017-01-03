function [afixed,sqnorm,ierr] = lsearch (afloat,L,D,Chi2,ncands)
%LSEARCH: Integer ambiguity resolution, search
%
% This routine finds the integer vector which is closest to a given
% float vector, in a least squares sence. This is the search-step in
% integer ambiguity resolution. It is best to perform this search only
% on ambiguities which have been decorrelated using LAMBDA.
%
% Input arguments:
%    afloat : Float ambiguities (\hat{a})
%    L      : LtDL-decomposition of the decorrelated 
%    D        variance-covariance matrix of the ambiguities
%    Chi2   : Size of the search ellipsoid
%    ncands : Number of requested candidates
%
% Output arguments:
%    afixed : Estimated integers (matrix)
%    sqnorm : Corresponding squared norms (vector, sorted)
%    ierr   : Error code: 0: No errors found
%                         1: Not enough candidates found

% ----------------------------------------------------------------------
% File.....: lsearch.m
% Date.....: 19-MAY-1999
% Author...: Peter Joosten
%            Mathematical Geodesy and Positioning
%            Delft University of Technology
% ----------------------------------------------------------------------

% -------------------------------
% --- Initializing statements ---
% -------------------------------

Linv      = inv(L);
Dinv      = 1./D;

True      = 1;
False     = 0;

n         = max(size(afloat));

right     = [zeros(n,1) ; Chi2];
left      = [zeros(n+1,1)];
dq        = [Dinv(2:n)./Dinv(1:n-1) 1/Dinv(n)];

cand_n    = False;
c_stop    = False;
endsearch = False;

ncan      = 0;

i         = n + 1;
iold      = i;
ierr      = 0;

afixed = zeros(n,ncands);
sqnorm = zeros(1,ncands);

% ----------------------------------
% --- Start the main search-loop ---
% ----------------------------------

while ~ (endsearch);

   i = i - 1;

   if iold <= i
      lef(i) = lef(i) + Linv(i+1,i);
   else
      lef(i) = 0;
      for j = i+1:n;
         lef(i) = lef(i) + Linv(j,i)*dist(j,1);
      end;
   end;
   iold = i;
   
   right(i)  = (right(i+1) - left(i+1)) * dq(i);
   reach     = sqrt(right(i));
   delta     = afloat(i) - reach - lef(i);
   dist(i,1) = ceil(delta) - afloat(i);
   
   if dist(i,1) > reach - lef(i)

%     ----------------------------------------------------
%     --- There is nothing at this level, so backtrack ---
%     ----------------------------------------------------

      cand_n = False;
      c_stop = False;
      
      while (~ c_stop) & (i < n);
      
         i = i + 1;
         if dist(i) < endd(i);
            dist(i) = dist(i) + 1;
            left(i) = (dist(i) + lef(i)) ^ 2;
            c_stop = True;
            if i == n; cand_n = True; end;
         end;
      
      end;
      
      if (i == n) & (~ cand_n); endsearch = True; end;
      
   else

%     ----------------------------
%     --- Set the right border ---
%     ----------------------------

      endd(i) = reach - lef(i) - 1;
      left(i) = (dist(i,1) + lef(i)) ^ 2;

   end

   if i == 1;
   
%     -------------------------------------------------------------------
%     --- Collect the integer vectors and corresponding               ---
%     --- squared distances, add to vectors "afixed" and "sqnorm" if: ---                             ---
%     --- * Less then "ncands" candidates found so far                ---
%     --- * The squared norm is smaller than one of the previous ones ---
%     -------------------------------------------------------------------

      t       = Chi2 - (right(1)-left(1)) * Dinv(1);
      endd(1) = endd(1) + 1;
      
      while dist(1) <= endd(1);

         if ncan < ncands;
         
            ncan             = ncan + 1;
            afixed(1:n,ncan) = dist + afloat;
            sqnorm(ncan)     = t;

         else
         
            [maxnorm,ipos] = max(sqnorm);
            if t < maxnorm;
               afixed(1:n,ipos) = dist + afloat;
               sqnorm(ipos)     = t;
            end;
            
         end;

         t       = t + (2 * (dist(1) + lef(1)) + 1) * Dinv(1);
         dist(1) = dist(1) + 1;

      end;
      

%     -------------------------
%     --- And backtrack ... ---
%     -------------------------

      cand_n = False;
      c_stop = False;
      
      while (~ c_stop) & (i < n);

         i = i + 1;

         if dist(i) < endd(i);
            dist(i) = dist(i) + 1;
            left(i) = (dist(i) + lef(i)) ^ 2;
            c_stop = True;
            if i == n; cand_n = True; end;
         end;
      
      end;

      if (i == n) & (~ cand_n); endsearch = True; end;
      
   end;

end;

% ----------------------------------------------------------------------
% --- Sort the resulting candidates, according to the norm
% ----------------------------------------------------------------------

tmp    = sortrows ([sqnorm' afixed']);
sqnorm = tmp(:,1)';
afixed = tmp(:,2:n+1)';

% ------------------------
% --- Check for errors ---
% ------------------------

if ncan < ncands; ierr = 1; end;

% ----------------------------------------------------------------------
% End of routine: lsearch
% ----------------------------------------------------------------------
