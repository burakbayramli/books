function [u,v] = updateVelocity(imax,jmax,u_star,v_star,p_prime,d_u,d_v,velocity)
v = zeros(imax,jmax+1);
u = zeros(imax+1,jmax);

%update interior nodes of u and v
for i=2:imax
    for j=2:jmax-1
        
        u(i,j) = u_star(i,j) + d_u(i,j)*(p_prime(i-1,j)-p_prime(i,j));
        
    end
end

for i=2:imax-1
    for j=2:jmax
        
        v(i,j) = v_star(i,j) + d_v(i,j)*(p_prime(i,j-1)-p_prime(i,j));
        
    end
end

%update BCs
v(1,1:jmax+1) = 0.0; %left wall
v(imax,1:jmax+1) = 0.0; %right wall
v(1:imax, 1) = -v(1:imax, 2); %bottom wall
v(1:imax, jmax+1) = -v(1:imax, jmax); %top wall 

u(1,1:jmax) = -u(2,1:jmax); %left wall
u(imax+1,1:jmax) = -u(imax,1:jmax); %right wall
u(1:imax+1, 1) = 0.0; %bottom wall
u(1:imax+1, jmax) = velocity; %top wall 

return
end
