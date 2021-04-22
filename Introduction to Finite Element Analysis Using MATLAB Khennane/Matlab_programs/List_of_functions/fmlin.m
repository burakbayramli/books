function[der,fun] = fmlin(samp, ig,jg)
%
% This function returns the vector of the shape function and their 
% derivatives with respect to xi and eta 
%
xi=samp(ig,1);
eta=samp(jg,1);
%
fun = 0.25*[(1.- xi - eta + xi*eta);...
            (1.+ xi - eta - xi*eta);...
            (1.+ xi + eta + xi*eta);...
            (1.- xi + eta - xi*eta)];
%
der = 0.25*[-(1-eta)    (1-eta)    (1+eta)   -(1+eta);...
            -(1-xi)     -(1+xi)    (1+xi)     (1-xi)]; 
% end function fmlin
