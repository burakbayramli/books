
%
%  m-file to compute the first 5 values of the 
%  inverse z-transform using the power series method 
%  (Program 4D.1, p235; program name: prog4d1.m)
%
n = 5;	% number of power series points
N1 = [1 -1.122346 1]; D1 = [1  -1.433509 0.85811];
N2 = [1 1.474597 1]; D2 = [1 -1.293601 0.556929];
N3 = [1 1 0]; D3 = [1 -0.612159 0];
B = [N1; N2; N3];	A = [D1; D2; D3];
[b,a] = sos2tf([B A]);
b = [b zeros(1,n-1)];
[h,r] = deconv(b,a);	%perform long division
disp(h);

