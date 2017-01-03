function h = bar3zcolor(Z);
%BAR3ZCOLOR Plot a 3D bar plot of the matrix Z.
% h = bar3zcolor(Z)
% The vertical height is contained in entry Z(i,j)
h=bar3(Z);
for i=1:length(h)
    zdata=get(h(i),'Zdata');
    set(h(i),'Cdata',zdata);
end