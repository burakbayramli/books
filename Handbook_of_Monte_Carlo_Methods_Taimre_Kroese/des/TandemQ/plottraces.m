%TandemQ/plottraces.m
figure,subplot(2,1,1),
for i =1:length(xx)-1,
    line([tt(i),tt(i+1)],[xx(i),xx(i)]);
    line([tt(i+1),tt(i+1)],[xx(i),xx(i+1)]);
end
aa=axis;
axis([0,tt(end),aa(3),aa(4)]),xlabel('t'),ylabel('Queue 1')

subplot(2,1,2),
for i =1:length(yy)-1,
    line([tt(i),tt(i+1)],[yy(i),yy(i)]);
    line([tt(i+1),tt(i+1)],[yy(i+1),yy(i)]);
end
aa=axis;
axis([0,tt(end),aa(3),aa(4)]),xlabel('t'),ylabel('Queue 2'),
