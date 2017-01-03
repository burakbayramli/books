function [out pottype]=potistyped(pot)
% [out pottype]=POTISTYPED(pot)
% out(i)=1 if potential pot(i) is not a table
% pottype(i) is the type of potential pot(i)

%out=0;pottype{1}='';
for i=1:length(pot)
    if isfield(pot(i).table,'type')
        out(i)=1;
        pottype{i}=pot(i).table.type;
    else
        pottype{i}='table';
        out(i)=0;
    end
end