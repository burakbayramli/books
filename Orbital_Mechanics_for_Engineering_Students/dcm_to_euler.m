% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function [alpha beta gamma] = dcm_to_euler(Q)
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function finds the angles of the classical Euler sequence
  R3(gamma)*R1(beta)*R3(alpha) from the direction cosine matrix

  Q     - direction cosine matrix
  alpha - first angle of the sequence (deg)
  beta  - second angle of the sequence (deg)
  gamma - third angle of the sequence (deg)

  User M-function required: atan2d_0_360
%}
% -----------------------------------------------

alpha = atan2d_0_360(Q(3,1), -Q(3,2));
beta  = acosd(Q(3,3));
gamma = atan2d_0_360(Q(1,3), Q(2,3));

end
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~