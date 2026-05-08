function Sij = compute_Sij_term(df,i,j,omega)

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

Sij = zeros(size(df,1),size(df,2));
for k = 1:9
    Sij = Sij + df(:,:,k) * c(k,i) * c(k,j);
end
Sij = 3*omega*Sij;