% generate the element stiffness matrix for each element
function ke = trusselem(e)
include_flags;


const = CArea(e)*E(e)/leng(e);    % constant coefficient for each truss element

if ndof == 1
    ke = const * [1   -1 ;        % stiffness matrix for 1D
                 -1    1];
             
elseif ndof == 2
    p = phi(e)*pi/180;       % converts degrees to radians
   
    s   = sin(p);        c  = cos(p);
    s2  = s^2;           c2 = c^2;
    
    ke = const*[c2      c*s    -c2     -c*s;       % stiffness matrix for 2D
                c*s     s2     -c*s     -s2;
               -c2     -c*s     c2       c*s;
               -c*s    -s2      c*s      s2];
           
end
