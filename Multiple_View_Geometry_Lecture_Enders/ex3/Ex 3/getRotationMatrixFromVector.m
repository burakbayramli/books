function R = getRotationMatrixFromVector(w)
  length_w = norm(w);
  w_hat = [0 -w(3) w(2); w(3) 0 -w(1); -w(2) w(1) 0];
  R = eye(3,3) + w_hat/length_w * sin(length_w) ...
               +(w_hat^2)/(length_w^2) * (1 - cos(length_w));
end