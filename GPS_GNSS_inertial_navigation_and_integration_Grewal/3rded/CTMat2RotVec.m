function rotvec = CTMat2RotVec(CTMat);
%
% Converts coordinate transformation matrix to rotation vector
% 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
%
% 1. Compute antisymmetric part of CTMat to get sine of theta
%
A          = (1/2)*(CTMat - CTMat');
sine_theta = sqrt(A(3,2)^2+A(1,3)^2+A(2,1)^2);
%
% 2. Solve for theta using Eq. C.125
%
theta = atan2(sine_theta,(CTMat(1,1)+CTMat(2,2)+CTMat(3,3)-1)/2);
%
% 3. Use truncated Taylor series approximation
%    near theta = 0 for theta/sin(theta)
%    and symmetric part of CTMat near theta = pi
%    in alternative formula to avoid computing theta/sin(theta) 
%
if sine_theta < .001
    if theta < .01
        thetaoversintheta = 1+1/6*theta^2+7/360*theta^4;
        rotvec = thetaoversintheta*[CTMat(3,2);CTMat(1,3);CTMat(2,1)];
    else
        %
        % Compute symmetric part of CTMat
        %
        S = (1/2)*(CTMat+CTMat');
        %
        % Use Eq. C.133ff for solution
        %
        % This can still give different sign on rhovec if theta = pi,
        % but rhovec and -rhovec represent the same rotation under
        % those conditions.  This is a problem with rotation vector
        % representation, not with this function.
        %
        % This implementation uses sign(A(3,2)) etc. to get sign right
        % on u(1) etc.  This always give correct answer, but the output
        % rotation vector may still have an ambiguity that causes no error.
        %
        if (S(1,1) >= S(2,2)) & (S(1,1) >= S(3,3))
            if A(3,2) == 0 s1 = 1; else s1 = sign(A(3,2)); end;
            u(1,1) = s1*sqrt((S(1,1)+1)/2);
            u(2,1) = S(1,2)/u(1,1)/2;
            u(3,1) = S(1,3)/u(1,1)/2;
        elseif (S(2,2) >= S(1,1)) & (S(2,2) >= S(3,3))
            if A(1,3) == 0 s2 = 1; else s2 = sign(A(1,3)); end;
            u(2,1) = s2*sqrt((S(2,2)+1)/2);
            u(1,1) = S(1,2)/u(2,1)/2;
            u(3,1) = S(2,3)/u(2,1)/2;
        else
            if A(2,1) == 0 s3 = 1; else s3 = sign(A(2,1)); end;
            u(3,1) = s3*sqrt((S(3,3)+1)/2);
            u(1,1) = S(1,3)/u(3,1)/2;
            u(2,1) = S(2,3)/u(3,1)/2;
        end;
        rotvec = theta*u;
    end;
else
    rotvec = theta/sine_theta*[A(3,2);A(1,3);A(2,1)];
end;
%
% 4. Convert rotation vector modulo 2*Pi to be of length <= pi
%
thetaRaw    = sqrt(rotvec(1)^2+rotvec(2)^2+rotvec(3)^2);
thetaMod2Pi = mod(thetaRaw,2*pi);
if thetaMod2Pi>pi thetaMod2Pi=thetaMod2Pi-2*pi; end;
theta       = thetaMod2Pi;
rotvec      = (theta/thetaRaw)*rotvec;
return;