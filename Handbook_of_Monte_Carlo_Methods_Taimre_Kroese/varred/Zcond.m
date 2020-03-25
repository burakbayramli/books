function Z=Zcond(u)
a=[1,2,3,1,2]; 
X = u*diag(a);
Z = [min([X(:,4), X(:,3) + X(:,5)],[],2),...
    min([X(:,5), X(:,3) + X(:,4)],[],2)];
