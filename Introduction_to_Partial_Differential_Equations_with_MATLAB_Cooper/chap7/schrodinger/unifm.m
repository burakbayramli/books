

%                          Program unif (uniform density)
%
%  Computes and plots the solution of the free Schrodinger equation
%  with initial data being the function  with constant value
%  1/sqrt(2) on the interval [-1,1].  Uses Riemann sum (mid point rule)
%  as in program heat1.  Requires function mfile bigg.m for the fundamental
%  solution of the the Schrodinger equation. 
%  User enters time t when solution is to be viewed, and the number of terms
%  in the Riemann sum. The solution vector u is complex. Re(u) is plotted
%  in yellow, Im(u) is plotted in magenta, and the probability density rho
%  is plotted in cyan, all on the interval [0,20].

t  = input('enter the time t   ')
n = input( 'enter the number of terms to be summed   ')

    dely = 2/n;
    y = -1-dely/2: dely : 1-dely/2 ;

    x =0: .05 : 20;
    sum = zeros(size(x));

    for i = 1:n 
       sum = sum + bigg(x - y(i), t);
    end
    u  = dely*sum/sqrt(2);

    rho = (real(u)).^2 + (imag(u)).^2;


    

    plot(x,real(u),x,imag(u),x,rho )
        
