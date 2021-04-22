function[bee, radius] = formbee_axi(deriv,nne,fun, coord,eldof)
%
% This function assembles the matrix [bee] for an axisymmetric
% problem from the derivatives of the shape functions in global 
% coordinates
%
bee=zeros(4,eldof);
%
radius = dot(fun,coord(:,1));
%
for m=1:nne
    k=2*m;
    l=k-1;
    x=deriv(1,m);
    bee(1,l)=x;
    bee(4,k)=x;
    y=deriv(2,m);
    bee(2,k)=y;
    bee(4,l)=y;
    bee(3,l) = fun(m)/radius;
end
%
% End function formbee_axi
    