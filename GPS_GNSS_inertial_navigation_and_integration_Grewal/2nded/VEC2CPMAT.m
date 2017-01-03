function CPMAT = VEC2CPMAT(VEC)
%
% Converts column 3-vector VEC to 3x3 cross-product matrix CPMAT such that,
% for any column  3-vector V,
% 
%       cross(VEC,V) = VEC2CPMAT(VEC)*V
%
CPMAT = [0,VEC(3),-VEC(2);-VEC(3),0,VEC(1);VEC(2),-VEC(1),0];
return;