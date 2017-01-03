function solution8tile(path)
axis([-0.2,3.2,-0.2,3.2]);hold on
daspect([1,1,1]);
for i=1:3
    for j=1:3
        rectangle('Position',[i-1,j-1,1,1],'LineWidth',2);
    end
end
set(gca,'XTick',[],'YTick',[]);
handles=cell(3,3);

for i=1:length(path)
    [handles] = matrix2plot(reshape(path{i},3,3),handles);
    title(strcat('Move ',num2str(i-1)));
    pause(0.5);
end

end

function [handles]=matrix2plot(A,oldhandles)
handles=cell(size(A,1),size(A,2));
for i=1:size(A,1)
    for j=1:size(A,2)
        if (~isempty(oldhandles{i,j}))
            delete(oldhandles{i,j});
        end
        if A(i,j)>0
            handles{i,j}=text(j -0.6,size(A,1)-i + 0.5,num2str(A(i,j)),'FontSize',20,'FontWeight','bold');     
        end
    end
end
end