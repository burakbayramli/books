% ==============================
% Exercise 3.1.a:
function R = getRotationMatrixFromVector(w)
  length_w = norm(w);
  w_hat = [0 -w(3) w(2); w(3) 0 -w(1); -w(2) w(1) 0];
  R = eye(3,3) + w_hat/length_w * sin(length_w) ...
               +(w_hat^2)/(length_w^2) * (1 - cos(length_w));
end

% Exercise 3.1.b:  (compare Lecture-Slides 2, Slide 13)
function w = getVectorFromRotationMatrix(R)
  length_w = acos((trace(R)-1)/2);
  if (length_w == 0)
      w = [0 0 0]';
  else
      w = 1/(2*sin(length_w))*[R(3,2)-R(2,3) R(1,3)-R(3,1) R(2,1)-R(1,2)]'*length_w;
  end
end


% Exercise 3.1.c:
%               [ exp[w_hat]   (I - exp[w_hat]) * w_hat + w*w^T)/|w|)*v ]
% exp[xi_hat] = [                                                       ]
%               [    0^T                         1                      ]

function V = getTransformMatrixFromTwistCoord(twCoord)
  v = twCoord(1:3);
  w = twCoord(4:6);
  length_w = norm(w);
  w_hat = [0 -w(3) w(2); w(3) 0 -w(1); -w(2) w(1) 0];
  R = getRotationMatrixFromVector(w);
  T = ((eye(3,3) - R) * w_hat + w * w') * v / length_w;
  V = [R, T; zeros(1,3), 1];
end


%      [ v=(I-exp[w_hat])*w_hat+w*w^T)^{-1}*|w|*T ]
% xi = [                                          ]
%      [         w = hat^{-1}(log(w))             ]

function xi = getTwistCoordFromTransformMatrix(g)
  R = g(1:3,1:3);                                           % extract rotation matrix
  T = g(1:3,4);                                             % extract translation part
  w = getVectorFromRotationMatrix(R);                       % calc corresponding w
  %testR = getRotationMatrixFromVector(w);
  w_hat = [0 -w(3) w(2); w(3) 0 -w(1); -w(2) w(1) 0];       % we need w_hat as well
  v = inv((eye(3,3) - R) * w_hat + w * w') * norm(w) * T;   % calc the translation
  xi = [v; w];                                              % concatenate into one vector
end


