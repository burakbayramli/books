function [  ] = drawPts( img, pts )
hold off
imagesc(img);
colormap gray
hold on
plot(pts(:,1), pts(:,2), 'yo','LineWidth',3)
axis equal
end

