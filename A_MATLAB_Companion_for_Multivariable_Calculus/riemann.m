
%                 function  riemann 
%
%  Computes a Riemann sum and graphs stepwise approximation to function.
%  Call is  riemann(f,corners,graph) for an inline function f, and
%  riemann('f', corners, graph) for a function defined in an mfile.
%     corners = [a b c d] is a vector of the corner coordinates of a 
%  rectangle with corners (a,c), (b,c), (b,d), and (a,d). We assume
%  a < b and c < d. 
%     The third argument is optional. If some number is entered for 
%  graph, the graph of the stepwise approximation will be displayed.
%  If the third argument is omitted from the call, the graph is not
%  displayed. 
%      After the call, user is asked to enter the number n of 
%  subdivisions in the x direction, and the number m of subdivisions
%  in the y direction. 
%      The Riemann sums are computed using the midpoint of each
%  subrectangle. 

 
 function out = riemann(f, corners,graph)

 a = corners(1); b = corners(2); c = corners(3); d = corners(4);

 subdiv = input('enter the number of subdivisions in x and y  direction  as [n m] ')
 n = subdiv(1); m = subdiv(2);

  delx = (b-a)/n;  dely = (d-c)/m; 

  x = a:delx:b;
  y = c:dely:d;

  p = a +.5*delx: delx : b-.5*delx;
  q = c +.5*dely: dely : d-.5*dely;

  [P,Q] = meshgrid(p,q);
  Z = feval(f,P,Q);
  
  disp(' Approximate value of the integral ')
  out = sum(sum(Z))*delx*dely;

  if nargin < 3
     return
  end

  xplot = zeros(1,2*n);
  for j = 1:2:2*n-1
     xplot(j) = x((j+1)/2);
  end
  for j = 2:2:2*n
     xplot(j) = a + (j-2)*delx/2 + .99*delx;
  end


  yplot = zeros(1,2*m);
  for i = 1:2:2*m-1
      yplot(i) = y((i+1)/2);
  end
  for i = 2:2:2*m
      yplot(i) = c + (i-2)*dely/2 + .99*dely;
  end



  [Xplot,Yplot] = meshgrid(xplot, yplot);

  Zplot = zeros(size(Xplot));
  
  j = 1:2:2*n-1;
  k = 2:2:2*n;

  for i = 1:m
    Zplot(2*i-1,j) = Z(i,:);
    Zplot(2*i-1,k) = Z(i,:);
    Zplot(2*i, :) = Zplot(2*i-1,:);
  end

   surf(Xplot, Yplot, Zplot); colormap(cool)


