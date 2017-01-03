%LEV	 Least squares estimation of heights in a levelling
%	    network as described by the following 3 data files:
%
%	    levfix.dat, contains row-wise 2 columns
%	       point#	elevation
%
%	    levfree.dat, contains row-wise 1 column
%	       point#
%
%	    levobs.dat, contains row-wise 4 columns
%	       from-#	 to-#	    HDIFF      std. dev. of HDIFF
%
%      All units in meters

%Kai Borre 01-24-94
%Copyright (c) by Kai Borre
%$Revision: 1.0 $  $date: 1997/09/26 $

load levfix.dat
fix = levfix;
load levfree.dat
free = levfree;
load levobs.dat
obs = levobs;
[p,q] = size(fix);
[n,q] = size(free);
[m,q] = size(obs);
A = zeros(m,n);
cov = obs(:,4);
cov = diag(cov);

for i = 1:m
   t2 = 0; t4 = 0;
   for t1 = 1:p
      if obs(i,1) == fix(t1,1), break, end; 
      t1 = p+1;
   end
   if t1 == (p+1)
      for t2 = 1:n 
         if obs(i,1) == free(t2,1), break, end; 
      end
   end
   for t3 = 1:p
      if obs(i,2) == fix(t3,1), break, end; 
      t3 = p+1;
   end
   if t3 == (p+1)
      for t4 = 1:n
         if obs(i,2) == free(t4,1), break, end; 
      end
   end
   if t4 > 0 A(i,t4) = 1; end
   if t2 > 0 A(i,t2) = -1; end
   if t1 > 0 & t2 == 0 obs(i,3) = obs(i,3)+fix(t1,2); end
   if t3 > 0 & t4 == 0 obs(i,3) = obs(i,3)-fix(t3,2); end
end
obs(:,[1 2 4]) = [];
x = lscov(A,obs,cov);
sigma_0 = norm(A*x-obs)/sqrt(m-n)
Sigma_x = inv(A'*inv(cov)*A);
for i = 1:n
   sigma(i,1) = sigma_0*sqrt(Sigma_x(i,i)); 
end
disp('     Point'), disp(' Elevation    Standard deviation')
disp('--------------------------------')
for i = 1:n
   disp([free(i)]), disp([x(i) sigma(i)])
end
%%%%%%%%%%%%%%% end lev.m %%%%%%%%%%%%%%%%%%%%%%%%
