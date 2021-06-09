function[bee] = formbee(deriv,nne,eldof)
%
%  This function assembles the matrix [bee] from the 
%  derivatives of the shape functions in global coordinates
%
bee=zeros(3,eldof);
for m=1:nne
    k=2*m;
    l=k-1;
    x=deriv(1,m);
    bee(1,l)=x;
    bee(3,k)=x;
    y=deriv(2,m);
    bee(2,k)=y;
    bee(3,l)=y;
end
%
% End function formbee
    