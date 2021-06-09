num=[10]
den=[1 7 12];
step(num,den);
hold on
impulse(num,den);
ylabel('The step and the impulse response');
title(' and the impulse response using transfer functions');
gtext('step response');
gtext('impulse response')
