% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function Q = dcm_from_q(q)
% ~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function calculates the direction cosine matrix
  from the quaternion.

  q - quaternion (where q(4) is the scalar part)
  Q - direction cosine matrix
%}
% ----------------------------------------------

q1 = q(1); q2 = q(2); q3 = q(3); q4 = q(4);

Q = [q1^2-q2^2-q3^2+q4^2,      2*(q1*q2+q3*q4),       2*(q1*q3-q2*q4);
         2*(q1*q2-q3*q4), -q1^2+q2^2-q3^2+q4^2,       2*(q2*q3+q1*q4);
         2*(q1*q3+q2*q4),      2*(q2*q3-q1*q4),  -q1^2-q2^2+q3^2+q4^2 ];
end %dcm_from_q
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~