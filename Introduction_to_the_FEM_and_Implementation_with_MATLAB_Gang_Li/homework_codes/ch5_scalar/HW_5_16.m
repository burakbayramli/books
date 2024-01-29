clear all;

% next block: model using linear elements
nodes1(:,:,1)=[1 0 1; 2 0 0.5; 3 0 0; 4 .5 1; 5 .5 .5; 6 .5 0; 7 1 1; 8 1 .5; 9 1 0];
elements1(:,:,1)=[1 1 2 5 4; 2 4 5 8 7; 3 2 3 6 5; 4 5 6 9 8];
nodalTemp1(:,:,1)=[1 -1; 2 -1/8; 3 0; 6 -1/8; 9 -1];
edgeFlux1(:,:,1)=[1 4 0.75 -3; 2 3 0.75 6; 2 4 6 0.75; 4 3 -3 0.75];

% next block: model using quadratic elements
nodes2=[1 0 1; 2 0 0.5; 3 0 0; 4 .5 1; 5 .5 .5; 6 .5 0; 7 1 1; ...
          8 1 .5; 9 1 0; 10 0 .75; 11 0 .25; 12 .25 1; 13 .25 .5;...
          14 .25 0; 15 .5 .75; 16 .5 .25; 17 .75 1; 18 .75 .5;...
          19 .75 0; 20 1 .75; 21 1 .25];
elements2=[1 1 2 5 4 10 13 15 12; 2 4 5 8 7 15 18 20 17;...
                 3 2 3 6 5 11 14 16 13; 4 5 6 9 8 16 19 21 18];
nodalTemp2=[1 -1; 2 -1/8; 3 0; 6 -1/8; 9 -1; 10 -0.421875; ...
           11 -0.015625; 14 -0.015625; 19 -0.421875];
edgeFlux2=[1 4 0.75 -1.3125 -3; 2 3 0.75 3.1875 6; ...
          2 4 6 3.1875 0.75; 4 3 -3 -1.3125 0.75];

% next block: model using cubic elements
nodes3=[1 0 1; 2 0 0.5; 3 0 0; 4 .5 1; 5 .5 .5; 6 .5 0; 7 1 1; ...
          8 1 .5; 9 1 0; 10 0 5/6; 11 0 4/6; 12 0 2/6; 13 0 1/6;...
          14 1/6 1; 15 2/6 1; 16 1/6 .5; 17 2/6 .5; 18 1/6 0; ...
          19 2/6 0; 20 .5 5/6; 21 .5 4/6; 22 .5 2/6; 23 .5 1/6; ...
          24 4/6 1; 25 5/6 1; 26 4/6 .5; 27 5/6 .5; 28 4/6 0;...
          29 5/6 0; 30 1 5/6; 31 1 4/6; 32 1 2/6; 33 1 1/6];
elements3=[1 1 2 5 4 10 11 16 17 21 20 15 14;...
           2 4 5 8 7 20 21 26 27 31 30 25 24;...
           3 2 3 6 5 12 13 18 19 23 22 17 16;... 
           4 5 6 9 8 22 23 28 29 33 32 27 26];
nodalTemp3=[1 -1; 2 -1/8; 3 0; 6 -1/8; 9 -1; ...
            10 -0.578703703703704; 11 -0.296296296296296;...
            12 -0.037037037037037; 13 -0.004629629629630; ...
            18 -0.004629629629630; 19 -0.037037037037037;...
            28 -0.296296296296296; 29 -0.578703703703704];
edgeFlux3=[1 4 0.75 -2/3 -1.916666666666667 -3; ...
           2 3 0.75 7/3 4.083333333333334 6; ...
           2 4 6 4.083333333333334 7/3 0.75; ...
           4 3 -3 -1.916666666666667 -2/3 0.75];

% next block: exact solution
figure(1);
x=[0:0.05:1]';
y=ones(21,1)*0.25;
T=-x.^3 - y.^3 +3.*x.^2.*y + 3.*x.*y.^2;
hold off;
plot(x,T,'k-','linewidth',2);
hold on;
 
for k=1:3
  if k==1
    nodes= nodes1;
    elements= elements1;
    nodalTemp= nodalTemp1;
    edgeFlux= edgeFlux1;
  elseif k==2
    nodes= nodes2;
    elements= elements2;
    nodalTemp= nodalTemp2;
    edgeFlux= edgeFlux2;
  elseif k==3    
    nodes= nodes3;
    elements= elements3;
    nodalTemp= nodalTemp3;
    edgeFlux= edgeFlux3;
  end

  % next block: global material properties and constants
  kappa=ones(4,1);
  n_nodes=size(nodes,1);           % number of nodes
  n_nodalTemp=size(nodalTemp,1);   % number of nodal temperature
  
  % next block: compute K and F
  K=CompK516(nodes, elements, kappa);   % compute global K matrix
  F=CompF516(nodes, elements, kappa, edgeFlux); % global F
  
  % next block: apply temperature boundary condition
  coeff=abs(max(K(nodalTemp(1,1),:)))*1e8;  %penalty factor
  for i=1:n_nodalTemp
    node_id=nodalTemp(i,1);
    K(node_id, node_id)=coeff;
    F(node_id, 1) = nodalTemp(i,2)*coeff;
  end

  % solve the global linear system
  T=K\F;  
  
  % save the displacement results in file
  TOut=zeros(n_nodes,4);
  for n=1:n_nodes
    TOut(n,1:3)=nodes(n,1:3);
    TOut(n,4)=T(n,1);
  end
  TOut

  % next block: plot the results
  for e=3:4
    eta=[1:-0.1:-1]';
    xi=ones(21,1)*0;
    Te=T(elements(e,2:size(elements,2)));
    [element_nodes, node_id_map]= SetElementNodes(e, nodes, elements);
    if k==1
      [N,Nx,Ny]=CompNDNatPointsQuad4(xi, eta);
      Tt=Te'*N;
      xy=element_nodes'*N;
    elseif k==2
      [N,Nx,Ny]=CompNDNatPointsQuad8(xi, eta);
      Tt=Te'*N;
      xy=element_nodes'*N;
    elseif k==3
      [N,Nx,Ny]=CompNDNatPointsQuad12(xi, eta);
      Tt=Te'*N;
      xy=element_nodes'*N;
    end
    plot(xy(1,:),Tt,'-','color',[k*0.3 0.9/k .4],'linewidth',2);
  end
end
legend('Exact','Linear','','Quadratic','','Cubic','location','south');