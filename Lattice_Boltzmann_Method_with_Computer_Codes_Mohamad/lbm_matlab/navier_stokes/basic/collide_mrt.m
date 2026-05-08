function f = collide_mrt(f, u, v, rho, om)
% D2Q9 collisions on 2-d matrix.
% Multiple relaxation time formulation.
% om is omega, the relaxation frequency, or 1/tau (inverse of relaxation time).

% Constants
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
% S_vec = [1, 1.4, 1.4, 1, 1.2, 1, 1.2, om, om]'; % Mohamad.
S_vec = [1, 1.2, 1, 1, 1.2, 1, 1.2, om, om]'; % 2015 Zhang et al.
MinvS = Minv*diag(S_vec);
[rows, cols] = size(rho);

% Different orientation of vector indices
reorder = [1,2,6,3,7,4,8,5,9];
M_ = M(:,reorder);
Minv_coeff = 36*inv(M_);
% Minv_ = Minv_coeff / 36;
% S_vec_ = S_vec(reorder);
% MinvS_ = Minv_*diag(S_vec_);
% MinvS_ = Minv_*diag(S_vec);
% MinvS_coeff = 36*(Minv_*diag(S_vec));
S_vec2 = S_vec;
S_vec2([8,9]) = 1;
mdiff = Minv*diag(S_vec2) - MinvS;
mdiff_coeff = mdiff*36;

% Determine m - meq.
deltam = zeros(rows, cols, 9);
% First fill with m.
for k = 1:9
    for n = 1:9
        deltam(:,:,k) =  deltam(:,:,k) + M(k,n)*f(:,:,n); 
    end
end
% Now let us subtract meq.
ru = rho.*u;
rv = rho.*v;
u2 = u.^2;
v2 = v.^2;
uu = u2+v2;
deltam(:,:,1) = deltam(:,:,1) - rho;
% deltam(:,:,2) = deltam(:,:,2) - rho.*( -2 + 3*rho.*uu ); % Mohamad.
% deltam(:,:,3) = deltam(:,:,3) - rho.*( 1 - 3*rho.*uu ); % Mohamad.
deltam(:,:,2) = deltam(:,:,2) - rho.*(-2 + 3*uu); % 2015 Zhang et al.
deltam(:,:,3) = deltam(:,:,3) - rho.*(1 - 3*uu); % 2015 Zhang et al.
deltam(:,:,4) = deltam(:,:,4) - ru;
deltam(:,:,5) = deltam(:,:,5) + ru;
deltam(:,:,6) = deltam(:,:,6) - rv;
deltam(:,:,7) = deltam(:,:,7) + rv;
deltam(:,:,8) = deltam(:,:,8) - rho.*(u2-v2);
deltam(:,:,9) = deltam(:,:,9) - rho.*u.*v;
% Update f.
for k = 1:9
    for n = 1:9
        f(:,:,k) =  f(:,:,k) ...
            - MinvS(k,n) * deltam(:,:,n);
    end
end


