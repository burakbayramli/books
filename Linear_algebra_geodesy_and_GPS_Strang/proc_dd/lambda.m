function [a, disall] = lambda(fidlog, n, Q, a)
%LAMBDA    integer estimation with the LAMBDA method

% fidlog   file identification for log-file
% n        dimension of matrix Q and vector a
% Q        variance covariance matrix (symmetric)
%          Only the lower triangular part is stored and accessed
%          2-dimensional array, columnwise
% a        the vector with real valued estimates \hat{a} (float solution)
%          the vector with integer valued estimates \check{a}
%          (fixed solution)

% cands    2-dimensional array to store the candidates (n, MaxCan)
% disall   according squared norms \hat{a}-\check{a}
%          (sorted in increasing order)
% L        lower triangular matrix: Q = L^T*D*L
%          although L is lower triangular, L is stored column-wise in
%          a 2-dimensional array, to avoid the necessity of a dedicated
%          storage scheme.
% D        diagonal matrix
% Zti      inverse of transpose of Z-matrix
% v1       work array of length n
% ak       contains the (integer) increments of the original
%          float ambiguities

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

MaxCan = 2;
Zti = eye(n);     % initialize Z to a unit matrix

% make estimates in 'a' between -1 and +1 by subtracting an
% integer number, store the increments in ak (= shift the centre
% of the ellipsoid over the grid by an integer translation)
dout(fidlog, 'original float ambiguities', a, 1, n)
for i = 1:n
   v1(i) = rem(a(i),1);
   ak(i,1) = a(i)-v1(i);
   a(i) = v1(i);
end
dout(fidlog, ' ''reduced'' ambiguities', a, 1, n)
dout(fidlog, 'increments', ak, 1, n)

% make the L_1^{-T} D_1^{-1} L_1^{-1}  decomposition of
% the covariance matrix Q_hat{a} (equation (3.8), case 3)
[L, D] = ltdl(Q, n);
dout(fidlog, 'conditional variances (before)', D, 1, n)

% compute the Z-transformation based on L and D of Q, ambiguities
% are transformed according to \hat{z} = Z^T\hat{a}
[L, D, a, Zti] = re_order(n, L, D, a, Zti);
dout(fidlog, 'conditional variances (after)', D, 1, n)
fprintf(fidlog,'\n Z^T-matrix: \n');
Zt = inv(Zti);  % inversion yields transpose of Z (for output only)
for i = 1:n
   fprintf(fidlog,'\n');
   for j = 1:n
      fprintf(fidlog,'%8.0f', Zt(i,j));
   end
end
fprintf(fidlog,'\n');
dout(fidlog, 'transformed ''reduced'' float ambiguities', a, 1, n)

% For the search we need L and D of Q^{-1}, see section 4.1, or
% L^{-1} and D^{-1} of Q in here (in our case we use of course
% the LAMBDA-transformed L and D as they came from SRC1)
L = l_inv(n, L);
% ... and D_1
D = 1./D;
% find a suitable Chi^2 such that we have two candidates at minimum;
% use an eps to make sure the two candidates are inside the ellipsoid
eps = 1d-6;                   % some small number
v3 = chistart(n, D, L, a);
Chi_1 = v3(2)+eps;
% find the two candidates with minimum norm
[ncan, cands, disall, ipos] = search(Chi_1, MaxCan, n, a, D, L);
fprintf(fidlog,['\n %4.0f candidates found '...
                              'with a Chi^2 of %6.2f\n'],ncan, Chi_1);
dout(fidlog, 'fixed transformed ''reduced'' ambiguities', ...
                                                  cands(:,ipos), 1, n)
% back-transformation (and adding increments)
a = Zti*cands(:,ipos);
a = a+ak;
dout(fidlog,'a_check', a, 1, n)
% 'sort' the squared norms in increasing order
% (if ipos equals 2, the best candidate is in the second place:
% so reverse disall)
if ipos == 2
   h = disall(1);
   disall(1) = disall(2);
   disall(2) = h;
end
%%%%%%%%%%%%%%%%% end lambda.m  %%%%%%%%%%%%%%%%%%%%%%%
