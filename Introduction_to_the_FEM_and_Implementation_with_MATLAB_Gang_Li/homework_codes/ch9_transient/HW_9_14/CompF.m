% Compute global force vector
function F=CompF(nodes, elements, thickness, nx, ny, t)
n_nodes=size(nodes,1);
F=zeros(n_nodes*2,1);

x=t*50e3/3600;
dx=20/nx;
node1=(nx+1)*ny+1+floor(x/dx);
node2=(nx+1)*ny+1+ceil(x/dx);

if node2<=n_nodes
  if node1==node2
    F(2*node1,1)=2000*9.8;
  else
    F(2*node1,1)=-2000*9.8* (nodes(node2,2)-x)/dx/thickness;
    F(2*node2,1)=-2000*9.8* (x-nodes(node1,2))/dx/thickness;    
  end
end
