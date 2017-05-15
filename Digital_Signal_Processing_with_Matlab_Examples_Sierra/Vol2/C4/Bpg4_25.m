% Draw the quadtree
% use after the qadtree analysis
% analysis and plot of Q (the quadtree desciption)

figure(1)
plot([1 256],[1 1],'g'); hold on; %a bottom line
h=gca; set(h,'YDir','reverse');

sq=tfg(257:512,1:256); %LH square
n=size(sq,1); %side length

colormap('prism');
imagesc(sq);

Kmin=min(Q(:)); Kmax=max(Q(:));
for j=Kmax:-1:Kmin,
   sl=2^j; nsl=n/sl;
   for nx=0:nsl-1,
   for ny=0:nsl-1,
      %select a square
      wl=1+(nx*sl):((nx+1)*sl); %range x
      wc=1+(ny*sl):((ny+1)*sl); %range y
      if (Q(wl(1),wc(1)))==j, 
         %it is a leaf
         plot([wc(1) wc(1)],[wl(1) wl(end)],'k');
         plot([wc(end) wc(end)],[wl(1) wl(end)],'k');
         plot([wc(1) wc(end)],[wl(1) wl(1)],'k');
         plot([wc(1) wc(end)],[wl(end) wl(end)],'k');
      end
   end 
   end
end

axis([0 256 0 256]);
title('quadtree on image');