function y = mydet(A)
y=0;  %initialize y
[n, n] = size(A);  %record the size of the square matrix A
if n ==2
   y=mydet2(A);
   return
end
for i=1:n
   y=y+(-1)^(i+1)*A(1,i)*mydet(A(2:n, [1:(i-1) (i+1):n]));
end