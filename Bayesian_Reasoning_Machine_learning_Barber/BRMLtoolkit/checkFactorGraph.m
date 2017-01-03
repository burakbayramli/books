function N=checkFactorGraph(A)
if size(A,1)~=size(A,2);  error('Factor Graph adjacency matrix is not square'); end
N=size(A,1); 
V=min(find(A(1,:)))-1; % variables are first in the order
if V==0; error('Not a Factor Graph (no variables)'); end
if ~all(all(A(1:V,1:V)==0)) | ~all(all(A(V+1:end,V+1:end)==0)); error('Not a Factor Graph (there should be no links between variales or between factors)'); end
