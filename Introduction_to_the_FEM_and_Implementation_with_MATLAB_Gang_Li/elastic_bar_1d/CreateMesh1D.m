% Generate 1-D mesh 
% Input: start_x, end_x: x-coordinates of the starting and ending nodes
% Input: n_elements: number of elements 
% Output: saved data files: nodes.dat and elements.dat
function CreateMesh1D(start_x, end_x, n_elements)
dl=(end_x-start_x)/n_elements;     % element length
nodes=zeros(n_elements+1,2);       % empty nodes matrix
nodes(:,1)=1:n_elements+1;         % 1st column: global node index 
nodes(:,2)=start_x:dl:end_x;       % 2nd column: x-coordinates
% Save to nodes.dat
dlmwrite('nodes.dat',nodes,'delimiter','\t','precision','%.6f');
elements=zeros(n_elements,3);      % empty elements matrix 
elements(:,1)=1:n_elements;        % 1st column: element index 
elements(:,2)=1:n_elements;        % global node index of 1st node
elements(:,3)=2:n_elements+1;      % global node index of 2nd node
% Save to elements.dat
dlmwrite('elements.dat',elements,'delimiter','\t','precision','%d');