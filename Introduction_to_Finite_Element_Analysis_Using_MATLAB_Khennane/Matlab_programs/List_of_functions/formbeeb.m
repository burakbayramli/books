function[beeb] = formbeeb(deriv,nne,eldof)
%
%  This function assembles the matrix [beeb] from the 
%  derivatives of the shape functions in global coordinates
%  for a thick plate element (bending action)
%
beeb=zeros(3,eldof);
for m=1:nne
    k=3*m;
    j=k-1;
    x=deriv(1,m);
    beeb(1,j)=x;
    beeb(3,k)=x;
    y=deriv(2,m);
    beeb(2,k)=y;
    beeb(3,j)=y;
end
%
% End function formbeeb
    