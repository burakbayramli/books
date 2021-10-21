
% makes eps files for several plots used in the book.

PlotGrid = 1;
for Frame=[2 5 10]

    MappedGrid=1;
    plotframe2
    axis([0 2.5 0 2.5])
    eval(['print polarp' num2str(Frame) '.eps'])
    query

    MappedGrid=0;
    plotframe2
    axis([.5 2.5 0 pi/2])
    eval(['print polarc' num2str(Frame) '.eps'])
    query
    PlotGrid=0;
  end

