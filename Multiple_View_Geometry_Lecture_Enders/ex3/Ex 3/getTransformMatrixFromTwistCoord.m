function V = getTransformMatrixFromTwistCoord(twCoord)
  v = twCoord(1:3);
  w = twCoord(4:6);
  length_w = norm(w);
  w_hat = [0 -w(3) w(2); w(3) 0 -w(1); -w(2) w(1) 0];
  R = getRotationMatrixFromVector(w);
  T = ((eye(3,3) - R) * w_hat + w * w') * v / length_w;
  V = [R, T; zeros(1,3), 1];
end