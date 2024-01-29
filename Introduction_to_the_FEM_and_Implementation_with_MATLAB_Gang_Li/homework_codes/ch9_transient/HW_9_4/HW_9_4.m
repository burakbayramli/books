% following blocks: tetrahedral element

rho=2330;  % mass density

element_nodes=[10 7 1; 0 4 5; 8 6 8; 6 8 7]*0.01;
n_nodes_per_element=size(element_nodes,1);
Nv=zeros(n_nodes_per_element*3,3);

[gauss_points, gauss_weights]=GetTetraGauss(1);
[N,Nx,Ny,Nz]=CompNDNatPointsTetra4(gauss_points(:,1),gauss_points(:,2),...
                                gauss_points(:,3));
me=zeros(n_nodes_per_element*3, n_nodes_per_element*3);

for g=1:size(gauss_points,1)
    J=CompJacobian3DatPoint(element_nodes, Nx(:,g), Ny(:,g), Nz(:,g));
    detJ=det(J);
    for p=1:n_nodes_per_element
      Nv(3*p-2,1)=N(p,g);
      Nv(3*p-1,2)=N(p,g);
      Nv(3*p,3)=N(p,g);
    end
    me=me+Nv*Nv'*detJ*rho*gauss_weights(g);
end

me

lumped=diag(sum(me,2))

% following blocks: hexahedral element

element_nodes=[1 3 7; 11 3 7; 12 8 1; 3 7 2; 2 5 8; ...
               7 10 9; 8 12 2; 2 10 3]*0.01;
n_nodes_per_element=size(element_nodes,1);
Nv=zeros(n_nodes_per_element*3,3);

[gauss_points, gauss_weights]=GetHexaGauss(2,2,2);
[N,Nx,Ny,Nz]=CompNDNatPointsHexa8(gauss_points(:,1),gauss_points(:,2), ...
                                 gauss_points(:,3));
me=zeros(n_nodes_per_element*3, n_nodes_per_element*3);

for g=1:size(gauss_points,1)
    J=CompJacobian3DatPoint(element_nodes, Nx(:,g), Ny(:,g), Nz(:,g));
    detJ=det(J);
    for p=1:n_nodes_per_element
      Nv(3*p-2,1)=N(p,g);
      Nv(3*p-1,2)=N(p,g);
      Nv(3*p,3)=N(p,g);
    end
    me=me+Nv*Nv'*detJ*rho*gauss_weights(g);
end

me

lumped=diag(sum(me,2))