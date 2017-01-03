function ScaleFactor(b,a,nstep,size,structure)

% Obtain scale factors, using L and cheb norms, for transfer function
% either of canonic or of direct structure with single or multiple sections 

if structure
	norm0 = DirectScale(b,a,0,nstep); % obtain L1 norms
	norm1 = DirectScale(b,a,1,nstep); % obtain L2 norms
	norm2 = DirectScale(b,a,2,size);  % obtain Loo norms
else 
	norm0 = CanonicScale(b,a,0,nstep); % obtain L1 norms
	norm1 = CanonicScale(b,a,1,nstep); % obtain L2 norms
	norm2 = CanonicScale(b,a,2,size);  % obtain Loo norms
end
disp('L1-norms of the second order sections:'); disp(norm0);
disp('L2-norms of the second order sections:'); disp(norm1);
disp('Loo-norms of the second order sections:');disp(norm2);
