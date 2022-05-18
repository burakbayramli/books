%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%    An Introduction to Scientific Computing          %%%%%%%
%%%%%%%    I. Danaila, P. Joly, S. M. Kaber & M. Postel     %%%%%%%
%%%%%%%                 Springer, 2005                      %%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%==========================================
% Identification  and visualization 
% of the labels of the vertices
%==========================================
function [Irefd,CIrefd]=Show_ref(nfig)

global XYs Refs 

% Finds the vlaues for the labels of the vertices
Iref=Find_dif(Refs);

% Visualization of vertices with label>0 
% inside points   are supposed to be of label=0 

Irefd=Iref(find(Iref >0));

figure(nfig);hold on;
cols=[ 'c' 'y' 'b' 'm'];
cola=char('Cyan','Yellow','Blue','Magenta');
      
for i=1:length(Irefd)
 		 colori(i)=mod(i,length(cols))+1;
       iv=find(Refs == Irefd(i));
       plot(XYs(iv,1),XYs(iv,2),[cols(colori(i)) 'o'],'LineWidth',3);
end
drawnow;

updatej('Dirichlet boundaries (imposed temp) (Label/Color)')
for i=1:length(Irefd)
   updatej(['   ' num2str(Irefd(i)),'    ',cola(colori(i),:)]);
end

CIrefd=cols(colori);
hold off;