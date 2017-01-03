%nm831  
%Apply the power method to find the largest/smallest/medium eigenvalue 
clear
A= [2 0 1;0 -2 0;1 0 2];
x= [1 2 3]'; %x= [1 1 1]'; % with different initial vector
EPS=1e-8; MaxIter=100;
%the largest eigenvalue and its corresponding eigenvector
[lambda_max,v]= eig_power(A,x,EPS,MaxIter) 
%the smallest eigenvalue and its corresponding eigenvector
[lambda,v]= eig_power(A^-1,x,EPS,MaxIter);
lambda_min= 1/lambda,  v %Eq.(8.3-6)
%eigenvalue nearest to a number and its corresponding eigenvector
s= -3;  AsI= (A-s*eye(size(A)))^-1;
[lambda,v]= eig_power(AsI,x,EPS,MaxIter);
lambda= 1/lambda+s %Eq.(8.3-8)
fprintf('Eigenvalue closest to %4.2f=%8.4f\nwith eigenvector',s,lambda)
v
[V,LAMBDA]= eig(A) %modal matrix composed of eigenvectors
