	    % Rotation matrix from planet-fixed to local horizon frame
% delta: Latitude (rad.)
% lambda: Longitude (rad.)
function C=CLH(delta,lambda)
  C= [cos(delta)*cos(lambda) cos(delta)*sin(lambda) sin(delta);
      -sin(lambda) cos(lambda) 0;
      -sin(delta)*cos(lambda) -sin(delta)*sin(lambda) cos(delta)];
