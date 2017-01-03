function demoPCdata
%DEMOPCDATA demo of PC structure learning algorithm
load PCdata; figure
[vars nstates]=potvariables(pot);
A = dag(pot); [xcord,ycord]=draw_layout(A); title('oracle')
[G,S]=PCskeletonData(data(:,1:1000)); V=size(G,1);
figure; draw_layout(G,cellstr(int2str((1:V)')),zeros(1,V),xcord,ycord); title('skeleton')
GpDAG=PCorient(G,S);
figure; draw_layout(GpDAG,cellstr(int2str((1:V)')),zeros(1,V),xcord,ycord);
title('Partially ordered DAG from 1000 datapoints')