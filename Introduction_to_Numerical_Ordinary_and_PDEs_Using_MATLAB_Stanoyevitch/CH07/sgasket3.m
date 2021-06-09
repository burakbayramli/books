function sgasket3(V1,V2,V3,ngen)
%input variables: V1,V2,V3 are vertices of a triangle in the plane,
%written as row vectors, ngen is the generation of Sierpinski gasket 
%that will be drawn in medium gray color.
if ngen == 0
%Fill triangle
 fill([V1(1) V2(1) V3(1) V1(1)],...
 [V1(2) V2(2) V3(2) V1(2)], [.5 .5 .5])
   hold on
   else 
%recursively invoke the same function on three outer subtriangles
   sgasket3(V1, (V1+V2)/2, (V1+V3)/2, ngen-1)
   sgasket3(V2, (V2+V1)/2, (V2+V3)/2, ngen-1)
   sgasket3(V3, (V3+V1)/2, (V3+V2)/2, ngen-1)
end
