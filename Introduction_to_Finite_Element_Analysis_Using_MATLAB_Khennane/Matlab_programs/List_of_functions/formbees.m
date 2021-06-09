function[bees] = formbees(deriv,fun, nne,eldof)
%
%  This function assembles the matrix [bees] from the 
%  derivatives of the shape functions in global coordinates
%  for the shear action in a plate element
%
bees=zeros(2,eldof);
for m=1:nne
    k=3*m;
    j=k-1;
    i=k-2;
    x=deriv(1,m); y=deriv(2,m);
    bees(2,i)=-x;
    bees(1,i)=-y;
    bees(1,k) = fun(m);
    bees(2,j) = fun(m);
end
%
% End function formbees
    