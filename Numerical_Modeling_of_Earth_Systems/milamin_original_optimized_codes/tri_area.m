function area = tri_area(P)
%
% triangular area, give P as (3,:)
%
if(size(P,1) ~= 3)
    error('need three points')
end
L=[sqrt(sum((P(1,:)-P(2,:)).^2)) sqrt(sum((P(2,:)-P(3,:)).^2)) sqrt(sum((P(3,:)-P(1,:)).^2))];
L=sort(L);
% Area calculation with stabilized Heron's formula
area = sqrt( (L(3)+(L(2)+L(1)))*(L(1)-(L(3)-L(2)))*(L(1)+(L(3)-L(2)))*(L(3)+(L(2)-L(1))) )/4;
