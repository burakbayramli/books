% following blocks: quadrilateral element

rho=2330;  % mass density

element_nodes=[0.03 0.08; 0.03 0.02; 0.10 0.05; 0.07 0.10];
n_nodes_per_element=size(element_nodes,1);
Nv=zeros(n_nodes_per_element*2,2);

[gauss_points, gauss_weights]=GetQuadGauss(2,2);
[N,Nx,Ny]=CompNDNatPointsQuad4(gauss_points(:,1),gauss_points(:,2));
me=zeros(n_nodes_per_element*2, n_nodes_per_element*2);

for g=1:size(gauss_points,1)
    J=CompJacobian2DatPoint(element_nodes, Nx(:,g), Ny(:,g));
    detJ=det(J);
    for p=1:n_nodes_per_element
      Nv(2*p-1,1)=N(p,g);
      Nv(2*p,2)=N(p,g);
    end
    me=me+Nv*Nv'*detJ*rho*gauss_weights(g);
end

me

lumped=diag(sum(me,2))



% following blocks: triangular element

element_nodes=[0.06 0.08; 0 0.04; 0.10 0.02];
n_nodes_per_element=size(element_nodes,1);
Nv=zeros(n_nodes_per_element*2,2);

[gauss_points, gauss_weights]=GetTriGauss(3);
[N,Nx,Ny]=CompNDNatPointsTri3(gauss_points(:,1),gauss_points(:,2));
me=zeros(n_nodes_per_element*2, n_nodes_per_element*2);

for g=1:size(gauss_points,1)
    J=CompJacobian2DatPoint(element_nodes, Nx(:,g), Ny(:,g));
    detJ=det(J);
    for p=1:n_nodes_per_element
      Nv(2*p-1,1)=N(p,g);
      Nv(2*p,2)=N(p,g);
    end
    me=me+Nv*Nv'*detJ*rho*gauss_weights(g);
end

me

lumped=diag(sum(me,2))
