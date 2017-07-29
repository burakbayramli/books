function zprim = zprim2(t,z,flag,ub,up,cp,k,m)
%For use with M-files trajectory_2.m
zprim = zeros(6,1);
zprim(1) = ub*cos(m*z(2))+m/k*up*sin(k*(z(1)-cp*t))*sin(m*z(2));
zprim(2) = up*cos(k*(z(1)-cp*t))*cos(m*z(2));
zprim(3) = ub*cos(m*z(4))+m/k*up*sin(k*(z(3)-cp*t))*sin(m*z(4));
zprim(4) = up*cos(k*(z(3)-cp*t))*cos(m*z(4));
zprim(5) = ub*cos(m*z(6))+m/k*up*sin(k*(z(5)-cp*t))*sin(m*z(6));
zprim(6) = up*cos(k*(z(5)-cp*t))*cos(m*z(6));
