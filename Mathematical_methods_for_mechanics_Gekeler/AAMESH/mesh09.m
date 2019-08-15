function t = mesh09(p,e,SEGNR1,SEGNR2);
% Eckart Gekeler, Universitaet Stuttgart, Release 8.4.05
% Aussenrand und ev. ein Innenrand
% Delaunay-Triangulierung
t  = delaunay(p(1,:),p(2,:));
t = t';
t1 = test02(p,t); % ev. Reihenfolge aendern
% Bei nichtkonvexen Gebieten: Elemente ausserhalb streichen:
t = mesh27(p,e,t1,SEGNR1,SEGNR2);
