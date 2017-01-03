function drawJTree(jtpot,infostruct,varargin)
%DRAWJTREE plot a Junction Tree
% drawJTree(jtpot,infostruct,<varinf>)
for i=1:length(jtpot);
    if nargin==3;
        a = field2cell(varargin{1}(jtpot(i).variables),'name');
    else
        a = cellstr(num2str(jtpot(i).variables));
    end
    c=[];for j=1:length(a)-1; c =[c a{j} ' ']; end;  c =[c a{end}];
    label{i}=[num2str(i),' [',c,']'];
end
cla; draw_layout(infostruct.cliquetree,label);