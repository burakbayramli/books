function tm = chistart(n, D, L, a)
% CHISTART computes squared distance of partially rounded
%          float vectors to the float vector in the metric
%          of the covariance matrix.

% n      number of ambiguities
% D      diagonal matrix
% L      lower triangular matrix: Q^{-1} = L D L^T
%        although L is lower triangular, in this example
%        program L is stored column-wise in a 2-dimensional
%        array, to avoid the necessity of a dedicated
%        storage scheme.
% a      float solution
% tm     tm(1) is smallest squared norm, tm(2) one-but-smallest

% Delft Geodetic Computing Centre/LGR, Paul de Jonge
% copyright by Delft University of Technology, Faculty of Geodesy

% Recoded into MATLAB by Kai Borre 12-04-96

big = 10^10;      % some big number
dist = zeros(n,1);
e = zeros(n,1);
t = zeros(n,1);

% a-\hat{a} with a the nearest integer to \hat{a}
dist = round(a)-a;

% sum_j = i:n l_{ji} (a_j-\hat{a}_j)
for i = 1:n
   e(i) = L(i:n,i)'*dist(i:n);
end

% compute t_0 and partial norms
t_0 = 0;
for i = 1:n
   t_0 = t_0+D(i)*e(i)*e(i);
end

tm(1,1) = t_0;
tm(2,1) = big;
% for 1 to n take the second nearest integer to a_i
for i = 1:n
   if dist(i) < 0
      nabla =1;
      t(i,1) = t_0;
      for j =1:i
         t(i,1) = t(i,1) + D(j) * L(i,j) * (2*e(j)+L(i,j));
      end
   else
      nabla = -1;
      t(i,1) = t_0;
      for j = 1:i
         t(i,1) = t(i,1) - D(j) * L(i,j) * (2*e(j)-L(i,j));
      end
   end
   
   % find the second smallest squared norm
   if t(i,1) < tm(1,1)
      tm(2,1) = tm(1,1);
      tm(1,1) = t(i,1);
   elseif t(i,1) < tm(2,1)
      tm(2,1) = t(i,1);
   end
end
%%%%%%%%%%%%%%%%%% end chistart.m %%%%%%%%%%%%%%%%%%%%%%
