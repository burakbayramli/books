

function z = green(x,y,xi,eta)

% The Green's function for the unit disk, modified, to avoid division
% by zero. To plot, the user should choose values for xi and eta where
% the singularity is to be located. 


    num = (x.^2 + y.^2).*(xi.^2 + eta.^2) - 2*(x.*xi + y.*eta) + 1 ;
   
    denom = (x-xi).^2 + (y-eta).^2 + .000001;

    z = (1/(4*pi))*log(num./denom);
