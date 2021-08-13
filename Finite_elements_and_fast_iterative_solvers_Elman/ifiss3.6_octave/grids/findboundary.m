function [bound,mbound] = findboundary(bndxy,bnde,xy,mv)
%FINDBOUNDARY generates vector bound and matrix mbound
%   [bound,mbound] = findboundary(bndxy,bnde,xy,mv)
%   input
%          bndxy      coordinates of nodes defining domain boundary
%          bnde       indices in bndxy of boundary edges 
%          xy         vertex coordinates
%          mv         q2 element mapping matrix
% output:
%          bound      dirichlet boundary vertices
%          mbound:    dirichlet boundary elements
%
%   IFISS function: HCE; 24 December 2009.
% Copyright (c) 2009 D.J. Silvester, H.C. Elman, A. Ramage;
% Code written by M. Wu 2009

bound = [];
mbound = [];
for i = 1:size(bnde,1)
    if bnde(i,3)
    if bndxy(bnde(i,1),1) == bndxy(bnde(i,2),1)
        yl = min(bndxy(bnde(i,1),2),bndxy(bnde(i,2),2));
        yu = max(bndxy(bnde(i,1),2),bndxy(bnde(i,2),2));
        k = find(xy(:,1) == bndxy(bnde(i,1),1) & xy(:,2)<yu & xy(:,2)>yl);
        bound = [bound;k];
        for j = 1:size(mv,1)
            if any(mv(j,6) == k)
                mbound = [mbound;j 2];
            end
            if any(mv(j,8) == k)
                mbound = [mbound;j 4];
            end
        end        
    else
        xl = min(bndxy(bnde(i,1),1),bndxy(bnde(i,2),1));
        xu = max(bndxy(bnde(i,1),1),bndxy(bnde(i,2),1));
        k = find(xy(:,2) == bndxy(bnde(i,1),2) & xy(:,1)<=xu & xy(:,1)>=xl);
        bound = [bound;k];
        for j = 1:size(mv,1)
            if any(mv(j,5) == k)
                mbound = [mbound;j 1];
            end
            if any(mv(j,7) == k)
                mbound = [mbound;j 3];
            end
        end
    end
    end
end
bound = sort(bound);
