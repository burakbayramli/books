%Colormap for spectrograms
mapg1=ones(128,3); %space for the map, white color
for mn=1:16,
   mapg1(mn,:)=[1 1 1];
end;
for mn=17:32,
   mapg1(mn,:)=[1 1 1];
end;
for mn=33:48,
   mapg1(mn,:)=[0.9 0.9 1];
end;
for mn=49:64,
   mapg1(mn,:)=[1 0 1];
end;
for mn=65:80,
   mapg1(mn,:)=[1 0 0];
end;
for mn=81:96,
   mapg1(mn,:)=[0 0 1];
end;
for mn=97:112,
   mapg1(mn,:)=[0 0 0.5];
end;
for mn=113:128,
   mapg1(mn,:)=[0 0 0];
end;
