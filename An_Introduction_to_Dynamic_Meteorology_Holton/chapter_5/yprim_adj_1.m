function dy = yprim_adj_1(t,y,flag,cor,csq,k,alph)
% ode function for barotropic geostrophic adjustment problem 1
dy = zeros(3,1);
dy(1) = cor*y(2) -k*y(3) -alph*y(1);
dy(2) = -cor*y(1);
dy(3) =+csq*k*y(1);
