function f = collide_mrt(f, u, v, rho, om)
% D2Q9 collisions on 2-d matrix.
% Multiple relaxation time formulation.
% om is omega, the relaxation frequency, or 1/tau (inverse of relaxation time).

M = [ones(1,9);...
    -4, -ones(1,4), 2*ones(1,4);
    4, -2*ones(1,4), ones(1,4);
    0, 1, 0, -1, 0, 1, -1, -1, 1;
    0, -2, 0, 2, 0, 1, -1, -1, 1;
    0, 0, 1, 0, -1, 1, 1, -1, -1;
    0, 0, -2, 0, 2, 1, 1, -1, -1;
    0, 1, -1, 1, -1, zeros(1,4);
    zeros(1,5), 1, -1, 1 -1];
Minv = 1/36*[4*ones(9,1), M(2,:)', M(3,:)', 6*M(4,:)', 3*M(5,:)',...
    6*M(6,:)', 3*M(7,:)', 9*M(8,:)', 9*M(9,:)'];

S_vec = [1, 1.4, 1.4, 1, 1.2, 1, 1.2, om, om]'; % Mohamad.
% S_vec = [1, 1.2, 1, 1, 1.2, 1, 1.2, om, om]'; % 2015 Zhang et al.

MinvS = Minv*diag(S_vec);

% Collide
[rows, cols] = size(rho);
meq = zeros(rows, cols, 9);

ru = rho.*u;
rv = rho.*v;
u2 = u.^2;
v2 = v.^2;
uu = u2+v2;

m = zeros(rows, cols, 9);
for k = 1:9
    for n = 1:9
        m(:,:,k) =  m(:,:,k) + M(k,n)*f(:,:,n); 
    end
end

meq(:,:,1) = rho;
meq(:,:,2) = rho.*( -2 + 3*rho.*uu ); % Mohamad.
meq(:,:,3) = rho.*( 1 - 3*rho.*uu ); % Mohamad.
% meq(:,:,2) = rho.*(-2 + 3*uu); % 2015 Zhang et al.
% meq(:,:,3) = rho.*(1 - 3*uu); % 2015 Zhang et al.
meq(:,:,4) = ru;
meq(:,:,5) = -ru;
meq(:,:,6) = rv;
meq(:,:,7) = -rv;
meq(:,:,8) = rho.*(u2-v2);
meq(:,:,9) = rho.*u.*v;

f_check = f;
for k = 1:9
    for n = 1:9
        f_check(:,:,k) =  f_check(:,:,k) ...
            - MinvS(k,n) * ( m(:,:,n) - meq(:,:,n) ); 
    end
end

for j = 1:rows
    for i = 1:cols
        m_vec = M*reshape(f(j,i,:),9,1,1);
        delta = -reshape( ...
            MinvS*( ...
                m_vec - reshape(meq(j,i,:),9,1,1) ...
            ),1,1,9 ...
        );
        f(j,i,:) = f(j,i,:) + delta;
    end
end

sum(sum(sum((f_check-f).^2)))


