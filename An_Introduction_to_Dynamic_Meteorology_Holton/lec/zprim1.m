function zprim = zprim1(t,z,flag,ub,up,cp,k,m)
% for use with traj1.m
zprim = zeros(6,1);
zprim(1) = ub+m/k*up*sin(k*(z(1)-cp*t))*sin(m*z(2));
zprim(2) = up*cos(k*(z(1)-cp*t))*cos(m*z(2));
zprim(3) = ub+m/k*up*sin(k*(z(3)-cp*t))*sin(m*z(4));
zprim(4) = up*cos(k*(z(3)-cp*t))*cos(m*z(4));
zprim(5) = ub+m/k*up*sin(k*(z(5)-cp*t))*sin(m*z(6));
zprim(6) = up*cos(k*(z(5)-cp*t))*cos(m*z(6));
