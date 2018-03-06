% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function [yaw pitch roll] = dcm_to_ypr(Q)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function finds the angles of the yaw-pitch-roll sequence
  R1(gamma)*R2(beta)*R3(alpha) from the direction cosine matrix

  Q     - direction cosine matrix
  yaw   - yaw angle (deg)
  pitch - pitch angle (deg)
  roll  - roll angle (deg)

  User M-function required: atan2d_0_360
%}
% ---------------------------------------

yaw   = atan2d_0_360(Q(1,2), Q(1,1));
pitch = asind(-Q(1,3));
roll  = atan2d_0_360(Q(2,3), Q(3,3));      
end
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~