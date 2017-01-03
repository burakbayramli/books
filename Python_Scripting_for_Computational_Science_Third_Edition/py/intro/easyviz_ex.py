# plot with scitools.easyviz:
from scitools.easyviz import *  
# or from scitools.std import *
subplot(2, 1, 1)
plot(x, y,'o')
hold(True)
plot(x, y_line, 'r')
plot(x, a*x + b, 'b')
legend('data points', 'original line', 'fitted line')
xlabel('input data')
ylabel('response')
title('easyviz v1: ' + headline)
show()
hardcopy('tmp1.png')

subplot(2, 1, 2)
plot(x, y, 'o',
     x, y_line, 'r',
     x, a*x + b, 'b',
     legend=('data points', 'original line', 'fitted line'),
     xlabel='input',
     ylabel='response',
     title='easyviz v2: ' + headline,
     hardcopy='tmp2.ps')
