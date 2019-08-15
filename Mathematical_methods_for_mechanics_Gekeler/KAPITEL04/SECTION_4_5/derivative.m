function gradient = derivative(funfcn,x,flag,epsilon,parameter)
% calculates gradient of y = funfcn(x,flag,parmeter)

y1 = feval(funfcn,x,flag,parameter);
m = length(y1); n = length(x);
gradient = zeros(m,n);
for i = 1:n
   aux = zeros(n,1); aux(i) = epsilon;
   y2 = feval(funfcn,x+aux,flag,parameter);
   gradient(:,i) = (y2 - y1)/epsilon;  
end       
