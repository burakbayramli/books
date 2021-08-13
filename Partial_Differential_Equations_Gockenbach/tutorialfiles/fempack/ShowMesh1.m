function ShowMesh1(T,flag,lflag,noff)

% ShowMesh1(T,flag)
%
%   This function displays a triangular mesh T.  Unless the flag
%   is nonzero, free nodes are indicated by a 'o', and
%   constrained nodes by an 'x'.
%
%   For a description of the data structure T, see "help Mesh1".
%
%   The optional argument flag has the following effect:
%
%      flag=1: the triangles are labeled by their indices
%      flag=2: the nodes by their indices
%      flag=3: both the nodes and triangles are labeled
%      flag=4: the free nodes are labeled by their indices.
%      flag=5: the free and constrained nodes are labeled by
%              their indices.

if nargin<2
   flag=0;
end
if nargin<3
   lflag=2;
end
if nargin<4
   noff=0;
end

L=size(T.ElList,1);
M=length(T.NodeList);

% Display the triangles and label them, if desired.

for i=1:L

   plot([T.NodeList(T.ElList(i,1),1);T.NodeList(T.ElList(i,2),1);...
         T.NodeList(T.ElList(i,3),1);T.NodeList(T.ElList(i,1),1)],...
        [T.NodeList(T.ElList(i,1),2);T.NodeList(T.ElList(i,2),2);...
         T.NodeList(T.ElList(i,3),2);T.NodeList(T.ElList(i,1),2)],...
         'k-','LineWidth',lflag)
   hold on

   if flag==1 | flag==3
      cx=(T.NodeList(T.ElList(i,1),1)+T.NodeList(T.ElList(i,2),1)+...
          T.NodeList(T.ElList(i,3),1))/3;
      cy=(T.NodeList(T.ElList(i,1),2)+T.NodeList(T.ElList(i,2),2)+...
          T.NodeList(T.ElList(i,3),2))/3;
      text(cx,cy,int2str(i),'FontSize',14)
   end

end

% Display the nodes and label them, if desired.

if flag>1

   xlen=max(T.NodeList(:,1))-min(T.NodeList(:,1));
   ylen=max(T.NodeList(:,2))-min(T.NodeList(:,2));
   xe=xlen/sqrt(M)/4;
   ye=ylen/sqrt(M)/10;

end

for i=1:M

   if ~noff
      if T.NodePtrs(i)>0
         plot(T.NodeList(i,1),T.NodeList(i,2),'o')
      else
         plot(T.NodeList(i,1),T.NodeList(i,2),'x')
      end
   end

   if flag==2 | flag==3
      r=.6;
      text(T.NodeList(i,1)+xe,T.NodeList(i,2)+ye,int2str(i),'FontSize',14)
   end

   if flag==4 & T.NodePtrs(i)>0
      r=.6;
      text(T.NodeList(i,1)+xe,T.NodeList(i,2)+ye,int2str(T.NodePtrs(i)),...
           'FontSize',14)
   end

   if flag==5
      r=.6;
      text(T.NodeList(i,1)+xe,T.NodeList(i,2)+ye,int2str(T.NodePtrs(i)),...
           'FontSize',14)
   end

end

hold off
