function [L,U,P] = fm(L,U,P,k,a)
% L assumed to have unit diagonal

[m,n] = size(P);

mm1 = m - 1; U(:,k) = [];

for j=k:mm1,
  jp1 = j+1;
  sub = j:jp1;
  delta = L(jp1,j)*U(j,j) + U(jp1,j);
  if (abs(U(j,j)) >= abs(delta))
    rho = U(jp1,j)/U(j,j);
    upd = jp1:m;
    L(upd,j) = L(upd,j) + rho*L(upd,jp1);
    upd = jp1:mm1;
    U(jp1,j:mm1) = [0. U(jp1,upd) - rho*U(j,upd)];
  else
    rho = U(j,j)/delta;
    if m <= j+2
      upd = j+2:m;
      temp = L(jp1,j);
      l = L(upd,j) - temp*L(upd,jp1);
      L(upd,sub) = [L(upd,jp1)+rho*l l];
      upd = jp1:mm1;
      u = U(jp1,upd) + temp*U(j,upd);
      U(sub,upd) = [u; U(j,upd)-rho*u];
    end
    L(jp1,j) = rho;
    U(sub,j) = [delta; 0.];
    P(sub,:) = P([jp1 j],:);
    if j>1     % must permute the rows of L already done too
      upd = 1:j-1;
      L(sub,upd) = L([jp1 j],upd);
    end
  end
end

U(:,m) = L\(P*a);
return;

