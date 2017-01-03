% returns perspective projection of X
function xp = project(X)

xp(1,:) = X(1,:)./X(3,:);
xp(2,:) = X(2,:)./X(3,:);
xp(3,:) = X(3,:)./X(3,:);