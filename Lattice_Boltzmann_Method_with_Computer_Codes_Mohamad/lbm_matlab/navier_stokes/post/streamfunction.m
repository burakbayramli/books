function strf = streamfunction(u,v,rho)
% Streamfunction calculation for D2Q9.

[rows, cols] = size(u);
strf = zeros(rows,cols);
for i = 2:rows
    rho_av = 0.5*( rho(1,i-1) + rho(1,i) );
    strf(1,i) = strf(1,i-1) - 0.5*rho_av*( v(1,i-1) + v(1,i) );
    for j = 2:cols
        rho_m = 0.5 * ( rho(j,i) + rho(j-1,i) );
        strf(j,i) = strf(j-1,i) + 0.5*rho_m*( u(j-1,i) + u(j,i) );
    end
end