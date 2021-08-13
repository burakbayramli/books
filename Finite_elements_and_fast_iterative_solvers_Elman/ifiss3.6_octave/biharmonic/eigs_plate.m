%EIGS_PLATE Compute eigenvalues of clamped plate problem in square domain
%   IFISS scriptfile: DJS; 9 September 2018.
% Copyright (c) 2018 D.J. Silvester, P. Nadukandi
nvtx=length(xy(:,1)); ndof=4*nvtx;
%[V,D]=eig(full(Agal),full(Q));
[V,D]=eigs(Agal,Q,9,0);
[ee,kk]=sort(diag(D));
disp([ee(1:9)]);
V=V(:,kk);
% plot first few eigenvectors
eigs_figure, shgx
fprintf('done\n')
