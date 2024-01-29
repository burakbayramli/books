function [oprev,onext,dprev,dnext]=NeighborEdges(a)
global edges
oprev=edges(a,4); onext=edges(a,5);
dprev=edges(a,6); dnext=edges(a,7);