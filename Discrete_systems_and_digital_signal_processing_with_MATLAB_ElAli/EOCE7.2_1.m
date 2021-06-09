step=dsolve('D2y+7*Dy+12*y=10','y(0)=0','Dy(0)=0') 
impulse=diff(step); % taking the derivative of the step
 	axis([0 10 0 1.5]) % set the axis
impulse =simple(impulse) % to simplify
clf % to clear the screen
ezplot(impulse,[0 10])
ylabel('The impulse response')
