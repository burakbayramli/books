function demo1
% Test of Gaussian integration rules on interval
% a      : left end of integration interval
% b      : right end of integration interval
% DEGREE : degree of polynomial >= 3 !!!!
% [delta,eps] = [0,0] : Gauss rules
% [delta,eps] = [1,0] : a additional support point
% [delta,eps] = [0,1] : b additional support point
% [delta,eps] = [1,1] : a and b additional support points
clc, format short, format compact
% -- Parameters -------------------
DEGREE = 5, N = DEGREE+1; % Number of Nodes
a = 0; b = 1;
% ---------------------------------
type = 100;
while ~ismember(type,[1,2,3,4])
   type = input(' Which type?, (1,2,3,4) ');
end
%type = 4;
switch type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
case 1, disp(' type 1, delta = 0, eps = 0' )
   M = 2*N-1; % max. degree of exact integration
   [nodes,weights] = gauss_1(N);
   NUMBERNODES = length(nodes)
case 2, disp(' type 2, delta = 1, eps = 0 ')
   M = 2*N-2; % max. degree of exact integration
   [nodes,weights] = gauss_2(N);
   NUMBERNODES = length(nodes)
case 3, disp(' type 3, delta = 0, eps = 1 ')
   M = 2*N-2; % max. degree of exact integration
   [nodes,weights] = gauss_3(N);
   NUMBERNODES = length(nodes)
case 4, disp(' type 4, delta = 1, eps = 1 ')
   M = 2*N-3; % max. degree of exact integration
   [nodes,weights] = gauss_4(N);
   NUMBERNODES = length(nodes)
end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% -- Exact integration of random polynomial --
AA = rand(1,M+1); 
BB = fliplr([1:M+1]); CC = AA./BB;
% -- AA(1) highest term -------------------
INTEGRALB = CC(1); INTEGRALA = CC(1);
for I = 1:M
   INTEGRALB = INTEGRALB*b + CC(I+1);
   INTEGRALA = INTEGRALB*a + CC(I+1);
end
INTEGRALB = INTEGRALB*b; INTEGRALA = INTEGRALA*a;
INTEGRAL_EXACT = INTEGRALB - INTEGRALA;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
% -- Numerical integration of random polynomial -----
INTEGRAL_NUM = 0;
for I = 1:N
   pp = AA(1);
   for K = 2:length(AA), pp = pp*nodes(I) + AA(K); end
   INTEGRAL_NUM = INTEGRAL_NUM + weights(I)*pp;
end    
INTEGRAL_EXACT_NUM = [INTEGRAL_EXACT,INTEGRAL_NUM]
