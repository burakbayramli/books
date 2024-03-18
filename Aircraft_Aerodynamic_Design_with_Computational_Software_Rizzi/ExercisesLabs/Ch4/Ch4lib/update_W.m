function [handles]=update_W(handles)
ib2 = handles.Data.Nodes - 1;
rhs = handles.Data.rhs;
W   = handles.Data.W;
Wold = handles.Data.Wold;
W(2:ib2,:) = Wold(2:ib2,:) - rhs(2:ib2,:);
handles.Data.W = W;