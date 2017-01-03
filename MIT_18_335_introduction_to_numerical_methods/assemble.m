function K=assemble(n,varargin)
%ASSEMBLE Assemble global stiffness matrix for square.
%   K=ASSEMBLE(N,MATERIAL) creates the stiffmess matrix for the unit
%   square with N-by-N elements, or (N+1)-by-(N+1) node points. It calls
%   ELMATRIX to compute the local stiffness matrix, and uses the direct
%   stiffness method to assemble all local matrices into one global matrix.
%
%   There are NN=(N+1)^2 total node points, and they are numbered as
%   i+(j-1)*(N+1), where i,j are the indices in the x,y directions.
%   The 2*NN global degrees of freedom correspond to first all the 
%   x-displacements, then all the y-displacements.
%
%   Example (compute and animate 24 lowest eigenfunctions, without
%            the mass-matrix):
%      K=assemble(20);
%      [V,D]=eigs(K,24,'sa',struct('disp',0));
%      for ii=1:size(V,2)
%        disp(sprintf('lambda_%d = %g',ii,D(ii,ii)))
%        qdanim(V(:,ii))
%      end
%
%   See also: ELMATRIX, MKMODEL, QDPLOT.

%   Per-Olof Persson <persson@math.mit.edu>

k=elmatrix(1/n,varargin{:});

if 1
  % Fast method, vectorized
  [i11,j11]=ndgrid(1:n,1:n);    
  [i21,j21]=ndgrid(2:n+1,1:n);  
  [i12,j12]=ndgrid(1:n,2:n+1);  
  [i22,j22]=ndgrid(2:n+1,2:n+1);
  ii=[i11(:)+(n+1)*(j11(:)-1),i21(:)+(n+1)*(j21(:)-1), ...
      i12(:)+(n+1)*(j12(:)-1),i22(:)+(n+1)*(j22(:)-1)];
  ii=[ii,ii+(n+1)^2];              
  i0=repmat(ii,1,8)';              
  j0=reshape(repmat(reshape(ii',1,8*n^2),8,1),64,n^2);
  K=sparse(i0,j0,repmat(k,1,n^2),2*(n+1)^2,2*(n+1)^2);
else
  % More intuitive but slower
  K=spalloc(2*(n+1)^2,2*(n+1)^2,2*9*2*(n+1)^2);
  for i=1:n
    for j=1:n
      [ix,iy]=ndgrid([i,i+1],[j,j+1]);
      i1=ix(:)+(iy(:)-1)*(n+1);
      i2=[i1;i1+(n+1)^2];
      K(i2,i2)=K(i2,i2)+k;
    end
  end
end

K=(K+K')/2; % Symmetrize
