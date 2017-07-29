function dy = yprim_adj_2(t,y,flag,cor,csq,ls,alph)
% ode function for barotropic geostrophic adjustment problem 2
dy = zeros(3,1);
dy(1) = cor*y(2) -ls*y(3) -alph*y(1);
dy(2) = -cor*y(1);
dy(3) =-csq*ls*y(1);
