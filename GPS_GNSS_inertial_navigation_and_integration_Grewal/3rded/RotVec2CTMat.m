function CTMat = RotVec2CTMat(RotVec);
%
% Converts rotation vector to coordinate transformation matrix
% 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
%
% 1. Convert rotation vector modulo 2*Pi to be of length <= pi
%
thetaRaw    = sqrt(RotVec(1)^2+RotVec(2)^2+RotVec(3)^2);
thetaMod2Pi = mod(thetaRaw,2*pi);
if thetaMod2Pi>pi thetaMod2Pi=thetaMod2Pi-2*pi; end;
theta       = thetaMod2Pi;
RotVec      = (theta/thetaRaw)*RotVec;
theta       = abs(theta);
%
ct    = cos(theta);
%
% 2. Calculate r = (1 - cos(theta) ) / theta^2
%
if theta < .001 
    %
    % Use truncated Taylor series approximation for small theta
    %
    r = 1/2-1/24*theta^2+1/720*theta^4-1/40320*theta^6+1/3628800*theta^8-1/479001600*theta^10+1/87178291200*theta^12-1/20922789888000*theta^14+1/6402373705728000*theta^16;
else
    r = (1-ct)/theta^2;
end;
%
% 3. Calculate s = sin(theta)/theta
%
if theta < .001 
    %
    % Use truncated Taylor series approximation for small theta
    %
    s = 1-1/6*theta^2+1/120*theta^4-1/5040*theta^6+1/362880*theta^8-1/39916800*theta^10+1/6227020800*theta^12-1/1307674368000*theta^14+1/355687428096000*theta^16;
else
    s = sin(theta)/theta;
end;
%
% 4. Compute coordinate transformation matrix CTMat and return
%
CTMat = ct*eye(3) + r*RotVec*RotVec' + s*[0,-RotVec(3),RotVec(2);RotVec(3),0,-RotVec(1);-RotVec(2),RotVec(1),0];
return;