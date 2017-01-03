function CTMat = Quat2CTMat(Quat)
%
% Converts unit quaternion Quat (as column 4-vector) to equivalent
% coordinate transformation matrix CTMat
%
CTMat = (2*Quat(1)^2-1)*eye(3) + 2*Quat(2:4)*Quat(2:4)' + 2*Quat(1)*VEC2CPMAT(Quat(2:4));
return;