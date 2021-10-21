
rpsoln = readm('rpsoln',4);
rpsoln(1,1) = -100;
rpsoln(end,1) = 100;

for Frame=0:10
   plotframe2
   plot(t*rpsoln(:,1),rpsoln(:,2)) 
   hold on
   plot(xp,qaug,'ro')
   hold off
   axis([-2 2 .5 3.5])
   title(['time t = ' num2str(t)])
   query
   end 
