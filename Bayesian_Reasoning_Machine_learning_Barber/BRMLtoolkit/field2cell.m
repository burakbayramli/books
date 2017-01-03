function c=field2cell(struc,field)
%FIELD2CELL Place the field of a structure in a cell
% c=field2cell(struc,field)
for i=1:length(struc)
    c{i}=getfield(struc(i),field);
end