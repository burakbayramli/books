function v=placeobject(F,h,Gx,Gy)
%PLACEOBJECT Place the object F at position h in grid Gx,Gy
% v=placeobject(F,h,Gx,Gy);
m = round(h); m(m<1)=1; % find nearest position
v=zeros(Gy,Gx);
v(m(1):m(1)+size(F,1)-1,m(2):m(2)+size(F,2)-1)=F;
v=v(1:Gx,1:Gy); % cut to observable grid