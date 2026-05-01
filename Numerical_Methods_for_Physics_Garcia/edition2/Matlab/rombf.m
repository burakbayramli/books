function R = rombf(a,b,N,func,param)
%  Function to compute integrals by Romberg algorithm
%  R = rombf(a,b,N,func,param)
%  Inputs
%    a,b    Lower and upper bound of the integral
%    N      Romberg table is N by N
%    func   Name of integrand function in a string such as
%           func='errintg'.  The calling sequence is func(x,param)
%    param  Set of parameters to be passed to function
%  Output 
%     R     Romberg table; Entry R(N,N) is best estimate of
%           the value of the integral

%* Compute the first term R(1,1)
h = b - a;        % This is the coarsest panel size
np = 1;           % Current number of panels
R(1,1) = h/2 * (feval(func,a,param) + feval(func,b,param));

%* Loop over the desired number of rows, i = 2,...,N
for i=2:N

  %* Compute the summation in the recursive trapezoidal rule
  h = h/2;          % Use panels half the previous size
  np = 2*np;        % Use twice as many panels
  sumT = 0;
  for k=1:2:np-1    % This for loop goes k=1,3,5,...,np-1
    sumT = sumT + feval(func, a + k*h, param);
  end

  %* Compute Romberg table entries R(i,1), R(i,2), ..., R(i,i)
  R(i,1) = 1/2 * R(i-1,1) + h * sumT;   
  m = 1;
  for j=2:i
    m = 4*m;
    R(i,j) = R(i,j-1) + (R(i,j-1) - R(i-1,j-1))/(m-1);
  end
end
return;

