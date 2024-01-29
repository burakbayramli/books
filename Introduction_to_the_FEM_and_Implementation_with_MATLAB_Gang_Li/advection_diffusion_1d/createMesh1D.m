% Generate 1-D mesh, input: x-coordinates of the 
% starting and ending points (start_x and end_x, and 
% number of elements (n_elements). Output: nodes.dat 
% and elements.dat

function createMesh1D(start_x, end_x, n_elements)

dl=(end_x-start_x)/n_elements;
nodes=zeros(n_elements+1,2);
nodes(:,2)=start_x:dl:end_x;
dlmwrite('nodes.dat',nodes,'delimiter','\t','precision','%.6f');


elements=zeros(n_elements,3);
elements(:,1)=1:n_elements;
elements(:,2)=1:n_elements;
elements(:,3)=2:n_elements+1;
dlmwrite('elements.dat',elements,'delimiter','\t','precision','%d');
