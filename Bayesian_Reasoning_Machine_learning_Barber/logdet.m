function l=logdet(A)
%LOGDET Log determinant of a positive definite matrix computed in a numerically more stable manner
[u s v]=svd(A); 
l=sum(log(diag(s)+1.0e-20));