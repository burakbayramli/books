function dydt = rhsSys(t,y,flag)
% rhsSys  Right hand side vector for two, coupled, first order ODEs
dydt = [ -y(1)*exp(1-t) + 0.8*y(2);
          y(1) - y(2)^3];             %  a column vector
