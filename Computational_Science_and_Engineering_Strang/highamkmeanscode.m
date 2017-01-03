%2.9  highamkmeanscode.m

% KMEANS_EX.M  
% Illustrate k means in a case where
% objects fall into two well defined groups.
%
%   This code can be contrasted with  
%   the spectral clustering code clus_ex.m
%
%    Objects are placed in 2D 
%
%     Two centers A and B are chosen by the user, and 
%     an object goes into group A if it is closest to A, and
%     into group B if it is closest to B.
%
%     The center of mass for points in group A becomes the 
%     new center A, and the center of mass for points in
%     group B becomes the new center B. We then proceed iteratively.
%      
%  DJH -> GS.  This version: June 2007
%

clf

randn('state',100);

% generate points in two groups

N = 20;       

for i = 1:N/2,
    x(i) = 1 + 0.2*randn;   % near x=1
    y(i) = 1 + 0.2*randn;   % near y=1
end

for i = N/2+1:N,
    x(i) = 2 + 0.2*randn;   % near x = 2
    y(i) = 2 + 0.2*randn;   % near y =2
end

plot(x,y,'*','MarkerSize',10)

display(' Type in coordinates for two centers')

xa = input(' x coord for center A = ')
ya = input(' y coord for center A = ')
xb = input(' x coord for center B = ')
yb = input(' y coord for center B = ')

carry_on = 1;

while carry_on == 1

  pa = [xa;ya];
  pb = [xb;yb];

  A = [];   %
  B = [];   % Groups A and B start off empty
  for i  = 1:N
      p = [x(i);y(i)];
      dista = norm(p - pa);
      distb = norm(p - pb);
      if dista < distb
          A = [A,p];  % add to group A
      else
          B = [B,p];  % add to group B
      end
  end

  if isempty(A)|isempty(B) %% problem: one is empty
    disp('Terminate! One group is empty')
    carry_on = 0;
  else
    clf
    plot(A(1,:),A(2,:),'r*','MarkerSize',10)
    hold on
    plot(xa,ya,'ro','MarkerSize',10)
    plot(B(1,:),B(2,:),'g*','MarkerSize',10)
    plot(xb,yb,'go','MarkerSize',10)

    xa = mean(A(1,:));
    ya = mean(A(2,:));
    xb = mean(B(1,:));
    yb = mean(B(2,:));

    carry_on = input('Type 1 to continue  ')

   end
end

  


          
    




