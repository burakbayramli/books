% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
function q = q_from_dcm(Q)
% ~~~~~~~~~~~~~~~~~~~~~~~~
%{
  This function calculates the quaternion from the direction
  cosine matrix.

  Q - direction cosine matrix
  q - quaternion (where q(4) is the scalar part)

%}
% ----------------------------------------------

K3 = ...
[Q(1,1)-Q(2,2)-Q(3,3), Q(2,1)+Q(1,2), Q(3,1)+Q(1,3), Q(2,3)-Q(3,2);
 Q(2,1)+Q(1,2), Q(2,2)-Q(1,1)-Q(3,3), Q(3,2)+Q(2,3), Q(3,1)-Q(1,3);
 Q(3,1)+Q(1,3), Q(3,2)+Q(2,3), Q(3,3)-Q(1,1)-Q(2,2), Q(1,2)-Q(2,1);
 Q(2,3)-Q(3,2), Q(3,1)-Q(1,3), Q(1,2)-Q(2,1), Q(1,1)+Q(2,2)+Q(3,3)]/3;

[eigvec, eigval] = eig(K3);

[x,i] = max(diag(eigval));

q = eigvec(:,i);
end %q_from_dcm 
% ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~