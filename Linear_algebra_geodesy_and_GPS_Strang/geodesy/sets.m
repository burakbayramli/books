function sets(b);
%STATION ADJUSTMENT
%    The direction observations are written as a matrix. Each column
%    contains the observed values for the single direction in the
%    various rounds.
%    The reference direction is omitted.
%    Outputs are the adjusted directions, the orientation unknowns,
%    and the standard deviation of a direction observed with one
%    round.

%Kai Borre 01-03-94
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $Date: 1997/09/26  $

[s,r] = size(b);  % s rounds, r+1 directions
b = [zeros(1,s); b'];
b = b(:);	% b is the right hand side of the observation equations
r = r+1;
A(1:r,1:r+s) = [eye(r) -ones(r,1) zeros(r,s-1)]; % s=1
for i = 2:s-1
   A((i-1)*r+1:i*r,1:r+s) = [...
                      eye(r) zeros(r,i-1) -ones(r,1) zeros(r,s-i)]; 
end
A((s-1)*r+1:r*s,1:r+s) = [eye(r) zeros(r,s-1) -ones(r,1)];
B = A(:,1:r);
C = A(:,r+1:r+s);
BB = B*B'/s;
CC = C*C'/r;
E = eye(r*s);
directions = B'*b/s
orient_unknowns = C'*(E-BB)*b/r
sigma0 = sqrt(b'*(E-BB)*(E-CC)*b/((r-1)*(s-1)))
%%%%%%%%%%%%%%%%%%%%%% end sets.m  %%%%%%%%%%%%%%%%%%%%