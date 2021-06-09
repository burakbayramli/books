step=dsolve('D2y+2*Dy+4*y=1','y(0)=0','Dy(0)=0')
impulse=diff(step) % taking the derivative of the step 
 	axis([0 10 -1 1]) % set the axis
impulse =simple(impulse) % to simplify
clf % to clear the screen
ezplot(impulse,[0 10])
ylabel('The impulse response')
title('h(t)=1/3*sin(3^(1/2)*t)*3^(1/2)/exp(t)')
