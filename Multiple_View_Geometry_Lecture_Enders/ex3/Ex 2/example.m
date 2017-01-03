%% load & display model
[V,F,P] = openOFF('model.off');

%% close figure
close all;

%% display model again (possibly with changed vertex positions V)
%P = patch('Vertices', V, 'Faces', F, 'FaceVertexCData',0.3*ones(size(V,1),3));
W = rotation(V,50,0,25);
P = patch('Vertices', W, 'Faces', F, 'FaceVertexCData',0.3*ones(size(W,1),3));

axis equal;
shading interp;
camlight right;
camlight left;