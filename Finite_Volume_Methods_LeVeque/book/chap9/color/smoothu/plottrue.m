
for n=1:length(tspan)
   plot(x,q0(:,n))
   axis([0 2 -1.2 1.2])
   title(['q at time t = ' num2str(tspan(n))])
   query
   end

