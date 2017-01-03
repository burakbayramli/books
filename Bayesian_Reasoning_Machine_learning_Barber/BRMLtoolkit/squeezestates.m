function [newdata uniquestates]=squeezestates(data)
%SQUEEZESTATES form new data matrix with states numbered from 1 upwards
% [newdata uniquestates]=squeezestates(data)
% example:
% data=ceil(10*rand(2,5));
% [newdata uniquestates]=squeezestates(data)
% celldisp(uniquestates);
% see also count.m
for i=1:size(data,1);
    v=unique(data(i,:)); uniquestates{i}=v;
    for s=1:length(v); newdata(i,data(i,:)==v(s))=s; end
end