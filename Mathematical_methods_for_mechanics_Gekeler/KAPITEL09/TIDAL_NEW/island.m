function island(p,e)
% makes island green

I = find(e(5,:) == 3); LI = length(I);
RDISLAND = [e(1,I), e(2,I(LI))];
K = RDISLAND; K = [K,K(1)];
X = p(1,RDISLAND); Y = p(2,RDISLAND);
fill(X,Y,'g','erasemode','none'), hold on
