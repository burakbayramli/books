function f = neutralize_cells(f,inactive)
% Sets the distributions of non-fluid cells so that they do not affect
% adjacent fluid cell distributions unphysically.
% inactive(j) = i, where i is the maximum i index that is not active.

for j = 1:length(inactive)
    f(j,1:inactive(j),:) = 0;
    f(j,1:inactive(j),1) = 1;
end

