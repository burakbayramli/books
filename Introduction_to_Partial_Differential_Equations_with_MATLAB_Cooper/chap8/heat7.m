

function u = heat7(x,y,t)
%  Function which solves the heat equation in the square G of side pi
%  with u = 0 on three sides, and u(0,y,t) = sin(y), summing the 
%  eigen function expansion.  M is the number of the term in the sum
%  and must be provided as a parameter. 

    global M

    sum = sinh(pi-x)/sinh(pi);
    for m = 1: M
       sum = sum -(2/pi)*(m/(1+m^2))*sin(m*x)*exp(-(1+m^2)*t);
    end

    u = sin(y).*sum;
