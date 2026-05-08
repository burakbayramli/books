function f = collide(f, u, v, rho, omega)

% D2Q9 collisions on 2-d matrix.
w = zeros(9,1);
w(1) = 4/9;
w(2:5) = 1/9;
w(6:9) = 1/36;
c = zeros(9,2);
c(1,:) = [0, 0];
c(2,:) = [1, 0];
c(3,:) = [0, 1];
c(4,:) = [-1, 0];
c(5,:) = [0, -1];
c(6,:) = [1, 1];
c(7,:) = [-1, 1];
c(8,:) = [-1, -1];
c(9,:) = [1, -1];

% Collide
t1 = u.*u + v.*v;
for k = 1:9
    t2 = c(k,1)*u + c(k,2)*v;
    feq = w(k)*rho.*(1 + 3*t2 + 4.5*t2.^2 - 1.5*t1);
    f(:,:,k) = omega*feq+(1-omega)*f(:,:,k);
end