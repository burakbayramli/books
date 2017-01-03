function Q = quattrim(Q0)
%
% forces "near quaternion" matrix to be a quaternion matrix
%
Q = (Q0(1,1)+Q0(2,2)+Q0(3,3)+Q0(4,4))/4*eye(4) + (-Q0(1,2)+Q0(2,1)-Q0(3,4)+Q0(4,3))/4*[0,-1,0,0;1,0,0,0;0,0,0,-1;0,0,1,0] + (-Q0(1,3)+Q0(2,4)+Q0(3,1)-Q0(4,2))/4*[0,0,-1,0;0,0,0,1;1,0,0,0;0,-1,0,0] + (-Q0(1,4)-Q0(2,3)+Q0(3,2)+Q0(4,1))/4*[0,0,0,-1;0,0,-1,0;0,1,0,0;1,0,0,0]; 
%%  
%%  M. S. Grewal, A. P. Andrews, and C. G. Bartone
%%  Global Navigation Satellite Systems, Inertial Navigation, and Integration
%%  Wiley-Interscience, 2013
%%  
