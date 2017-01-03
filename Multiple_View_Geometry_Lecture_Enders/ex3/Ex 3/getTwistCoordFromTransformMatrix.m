function xi = getTwistCoordFromTransformMatrix(g)
  R = g(1:3,1:3);                                           % extract rotation matrix
  T = g(1:3,4);                                             % extract translation part
  w = getVectorFromRotationMatrix(R);                       % calc corresponding w
  %testR = getRotationMatrixFromVector(w);
  w_hat = [0 -w(3) w(2); w(3) 0 -w(1); -w(2) w(1) 0];       % we need w_hat as well
  v = inv((eye(3,3) - R) * w_hat + w * w') * norm(w) * T;   % calc the translation
  xi = [v; w];                                              % concatenate into one vector
end