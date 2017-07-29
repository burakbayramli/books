%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Plot our standard map background -- Mercator Projection of US
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gr = 0.8;                     % define gray for coastline and state boundaries
load coast;                   %data for coastlines

%figure(1);
clf                       
latw = [20 60]; lonw = [220 300]; %limits of lat and long on map

mapp = 'mercator';
axesm('mapprojection',mapp,'maplatlimit',latw,'maplonlimit',lonw)
plotm(coast,'color',[gr gr gr],'linewidth',2)  %plots coastline
displaym(worldlo('POline'))
displaym(usalo('stateborder'))
gridm; plabel; mlabel

