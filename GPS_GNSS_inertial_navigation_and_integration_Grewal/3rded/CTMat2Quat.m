function Quat = CTMat2Quat(CTMat)
%
% Converts coordinate transformation matrix to equivalent unit quaternion
% 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2012
%%  
%
%
% Solution depends on which is the largest of:
%
%    t(1) = 1 + CTMat(1,1) + CTMat(2,2) + CTMat(3,3)
%    t(2) = 1 + CTMat(1,1) - CTMat(2,2) - CTMat(3,3)
%    t(3) = 1 - CTMat(1,1) + CTMat(2,2) - CTMat(3,3) 
%    t(4) = 1 - CTMat(1,1) - CTMat(2,2) + CTMat(3,3) 
%
if CTMat(2,2) > -CTMat(3,3)              % t(1) > t(2)
    if CTMat(2,2) > CTMat(3,3)           % t(3) > t(4)
        if CTMat(1,1) > -CTMat(3,3)      % t(1) > t(2),t(3),t(4)
            s = 2*sqrt(1 + CTMat(1,1) + CTMat(2,2) + CTMat(3,3));
            Quat = [s/4;(CTMat(2,3)-CTMat(3,2))/s;(CTMat(3,1)-CTMat(1,3))/s;(CTMat(1,2)-CTMat(2,1))/s];
        else                             % t(3) >= t(1),t(2),t(4)
            s = 2*sqrt(1 - CTMat(1,1) + CTMat(2,2) - CTMat(3,3));
            Quat = [(CTMat(1,3)-CTMat(3,1))/s;-(CTMat(2,1)+CTMat(1,2))/s;-s/4;-(CTMat(3,2)+CTMat(2,3))/s];
        end;
    else                                 % t(4) >= t(3)
        if CTMat(1,1) > -CTMat(2,2)      % t(1) > t(2),t(3),t(4)
            s = 2*sqrt(1 + CTMat(1,1) + CTMat(2,2) + CTMat(3,3));
            Quat = [s/4;(CTMat(2,3)-CTMat(3,2))/s;(CTMat(3,1)-CTMat(1,3))/s;(CTMat(1,2)-CTMat(2,1))/s];
        else                             % t(4) >= t(1),t(2),t(3)
            s = 2*sqrt(1 - CTMat(1,1) - CTMat(2,2) + CTMat(3,3));
            Quat = [(CTMat(2,1)-CTMat(1,2))/s;-(CTMat(1,3)+CTMat(3,1))/s;-(CTMat(3,2)+CTMat(2,3))/s;-s/4];
        end;
    end;
else                                     % t(2) >= t(1)
    if CTMat(2,2) > CTMat(3,3)           % t(3) > t(4)
        if CTMat(1,1) > CTMat(2,2)       % t(2) >= t(1),t(3),t(4)
            s = 2*sqrt(1 + CTMat(1,1) - CTMat(2,2) - CTMat(3,3));
            Quat = [(CTMat(3,2)-CTMat(2,3))/s;-s/4;-(CTMat(2,1)+CTMat(1,2))/s;-(CTMat(1,3)+CTMat(3,1))/s];
        else                             % t(3) >= t(1),t(2),t(4)
            s = 2*sqrt(1 - CTMat(1,1) + CTMat(2,2) - CTMat(3,3));
            Quat = [(CTMat(1,3)-CTMat(3,1))/s;-(CTMat(2,1)+CTMat(1,2))/s;-s/4;-(CTMat(3,2)+CTMat(2,3))/s];
        end;
    else                                 % t(4) >- t(3)
        if CTMat(1,1) > CTMat(3,3)       % t(2) > t(1),t(3),t(4)
            s = 2*sqrt(1 + CTMat(1,1) - CTMat(2,2) - CTMat(3,3));
            Quat = [(CTMat(3,2)-CTMat(2,3))/s;-s/4;-(CTMat(2,1)+CTMat(1,2))/s;-(CTMat(1,3)+CTMat(3,1))/s];
        else                             % t(4) >= t(1),t(2),t(3)
            s = 2*sqrt(1 - CTMat(1,1) - CTMat(2,2) + CTMat(3,3));
            Quat = [(CTMat(2,1)-CTMat(1,2))/s;-(CTMat(1,3)+CTMat(3,1))/s;-(CTMat(3,2)+CTMat(2,3))/s;-s/4];
        end;
    end;
end;
Quat = sign(Quat(1))*Quat;
return;