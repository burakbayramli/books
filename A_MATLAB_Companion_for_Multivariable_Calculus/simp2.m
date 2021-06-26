
%               Function simp2
%
%  Two dimensional Simpson's rule over a rectangle.
%  The call is simp2(f,corners) when f is given as an inline function,
%  and simp2('f',corners) when f is given in an mfile. 
%      corners = [a b c d] is a vector of corner coordinates of the
%  rectangle with corners (a,c), (b,c), (b,d), and (a,d). We assume
%  a< b and c < d.
%     After the call the user is asked to enter the number n of 
% subdivisions in the x direction, and the number m of the subdvision
% in the y direction.  n and m must be even. 

function out = simp2(f, corners)

 a = corners(1); b = corners(2); c = corners(3); d = corners(4);
 disp('  ')
 disp('the number of subivisions n and m in each direction must be even ')
 subdiv = input('enter the number of subdivisions in x and y direction as [n m] ')
 n = subdiv(1); m = subdiv(2);

 x = linspace(a,b, n+1); y = linspace(c,d,m+1);
 [X,Y] = meshgrid(x,y);
 
 svecx = 2*ones(size(x));
 svecx(2:2:n) = 4*ones(1,n/2);
 svecx(1) = 1; svecx(n+1) = 1;

 svecy = 2*ones(size(y));
 svecy(2:2:m) = 4*ones(1,m/2);
 svecy(1) = 1; svecy(m+1) = 1;

 S = svecy'*svecx;
 T = S.*feval(f,X,Y);
 disp('Approximate value of the integral using Simpsons rule ')
 out = sum(sum(T))*(b-a)*(d-c)/(9*m*n);


  
