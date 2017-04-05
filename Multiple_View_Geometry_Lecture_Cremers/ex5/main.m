
img = imreadbw('image.png');
img2 = imreadbw('image2.png');
std = 3;
kappa = 0.05;
th = 1e-7;
[score, pts] = getHarrisCorners(img, std, kappa, th);


subplot(1,2,1)
drawPts((score > 0).*abs(score).^0.25, pts);
colormap jet
subplot(1,2,2)
drawPts(img, pts);


%% get Velocity

[velx, vely] = getFlow(img, img2, std);

subplot(1,2,1)
imagesc(velx)
axis equal
subplot(1,2,2)
imagesc(vely)
axis equal


%% quiver plot
hold off
imagesc(img)
hold on
quiver(-velx*5, -vely*5)