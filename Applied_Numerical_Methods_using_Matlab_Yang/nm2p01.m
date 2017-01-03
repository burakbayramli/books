%nm2p01.m
.. .. .. .. .. .. .. ..
time_on=0; time_off=0;
.. .. .. .. .. .. .. ..
tic 
.. .. .. .. .. .. .. ..
time_on=time_on+toc;
tic
.. .. .. .. .. .. .. ..
time_off=time_off+toc;
.. .. .. .. .. .. .. ..
solutions=[xk_on xk_off]
discrepancy=norm(xk_on-xk_off)
times=[time_on time_off]
