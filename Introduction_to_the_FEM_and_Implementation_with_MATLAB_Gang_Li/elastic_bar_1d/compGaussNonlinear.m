% given the number of Gauss points, n_gauss_points, compute the positions
%and the weights of the Gauss points in the master (isoparametric) element
function [gauss_x, gauss_w] = compGauss(n_gauss_points, gauss_x, gauss_w)

  m=(n_gauss_points + 1)/2;
  for i=1:m 
    z=cos(pi*(i-0.25)/(n_gauss_points + 0.5));
    z1=z+1.0;
    while (abs(z-z1) > 1e-10)
      p1=1.0;
      p2=0.0;
      for j=1:n_gauss_points 
        p3=p2;
        p2=p1;
        p1=((2.0*j-1.0)*z*p2-(j-1.0)*p3)/j;
      end
      pp=n_gauss_points*(z*p1-p2)/(z*z-1.0);
      z1=z;
      z=z1-p1/pp;
    end 
    gauss_x(i)=-z;
    gauss_x(n_gauss_points-i+1)=z;
    gauss_w(i)=2.0/((1.0-z*z)*pp*pp);
    gauss_w(n_gauss_points-i+1)=gauss_w(i);
  end
