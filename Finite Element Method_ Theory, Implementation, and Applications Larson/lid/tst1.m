fd = @(p) -min(min(min(1+p(:,2),1-p(:,2)),1+p(:,1)),1-p(:,1));
fh = @(p) ones(size(p,1),1);
[p,t] = distmesh( fd, fh, 0.2, [-1,-1;1,1], [-1,-1;-1,1;1,-1;1,1] );
size(p)
size(t)
plot(p,'.')
