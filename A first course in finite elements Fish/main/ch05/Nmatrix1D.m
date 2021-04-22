% evaluates shape functions (in physical coordinates) at point xt
 function N = Nmatrix1D(xt,xe)
include_flags;

    if nen == 2         % linear shape functions       
        N(1) = (xt-xe(2))/(xe(1)-xe(2));
        N(2) = (xt-xe(1))/(xe(2)-xe(1));
    elseif nen == 3     % quadratic shape functions
        N(1)=(xt-xe(2))*(xt-xe(3))/((xe(1)-xe(2))*(xe(1)-xe(3)));
        N(2)=(xt-xe(1))*(xt-xe(3))/((xe(2)-xe(1))*(xe(2)-xe(3)));
        N(3)=(xt-xe(1))*(xt-xe(2))/((xe(3)-xe(1))*(xe(3)-xe(2)));
    end
