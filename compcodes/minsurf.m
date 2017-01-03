% 
%                   Function minsurf
%  
%     If Z is a matrix of heights of a surface over a meshgrid [X,Y]
%     minsurf uses the function tarea.m and the MATLAB function
%     fmins to find the new matrix W = minsurf(X,Y,Z) that agrees
%     with Z on the boundary, but which minimizes the surface area
%     as a function of the interior values of Z.  W = minsurf(X,Y,Z)
%     is the new matrix. The new matrix W may not provide the 
%     minimal surface. You may need to iterate minsurf several
%     times, taking W1 = minsurf(X,Y,W), W2 = minsurf(X,Y,W1), etc.
%         The input surface and the modified surface are graphed
%     together using tsurf for comparison.  
%         Depending on the machine, this program can run quite
%     slowly. It is best not to use more than 40 interior points.
%
%     Example: [X,Y] = meshgrid(0:.2:1)
%              Z = 0*X + 1;
%               Z(2:4, 2:4) = .5 + zeros(3,3);
%     is a surface that is equal to 1 on the boundary of the
%     unit square, and 0 at the interior points. The minimal
%     surface matrix is obviously W = 0*X +1. 
%
      
 function Zout = minsurf(X,Y,Z)

  n = size(X,2)-1;  m = size(X,1)-1;

  nn = n-1;

  dx = X(1,2) - X(1,1);  dy = Y(2,1) - Y(1,1);

  z0 = zeros(1,(n-1)*(m-1));
  for i = 2:m
     z0((i-2)*nn +1: (i-1)*nn) = Z(i, 2:n);
  end

  initial_area = tarea(z0,dx,dy,Z)

 % calculate the interior points which minimize the surface area.
  zmin = fmins('tarea', z0,[],[],dx,dy,Z);
  Zout = Z;

 % Replace the interior values of Z with the values of zmin.
  for i = 2:m 
     Zout(i,2:n) = zmin((i-2)*nn +1: (i-1)*nn);
  end

  final_area = tarea(zmin,dx,dy,Zout)

  h1 = min(min(Z)); h2 = max(max(Z));

  xmax = max(max(X)); xmin = min(min(X));
  ymax = max(max(Y)); ymin = min(min(Y));

  
  subplot(1,2,1)
    tsurf(X,Y,Z)
 title('original surface ')


 subplot(1,2,2)
    tsurf(X,Y,Zout)
    axis([xmin, xmax, ymin, ymax, h1,h2])
   
 title('minimal surface')







  

