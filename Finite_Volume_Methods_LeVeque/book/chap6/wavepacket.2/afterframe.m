hold on
plot(xtrue,qtrue)
axis([0 1 -1 1.5])
hold off

if Frame==0
   q0 = q;
   end

if Frame>0
   err1 = sum(abs(q-q0))*dx;
   errinf = max(abs(q-q0));
   disp([dx errinf err1])
   end
