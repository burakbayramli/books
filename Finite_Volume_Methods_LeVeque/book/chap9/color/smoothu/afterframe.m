hold on

if Frame==0
   disp('Computing true solution....')
   computetrue;
   end

qtrue = q0(:,Frame+1);   % assumes computetrue already called with tspan set
                         % properly.
plot(x,qtrue)
axis([0 4.0 -1.2 1.2])
hold off

if Frame>0
   err1 = sum(abs(q-qtrue))*dx;
   errinf = max(abs(q-qtrue));
   disp([dx errinf err1])
   end

