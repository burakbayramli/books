function y=D_sfilt(lo,hi,sf1,sf2)

% 2D synthesis filtering

%rows filtering
lo=D_1sf(lo,hi{1},sf2,2);
hi=D_1sf(hi{2},hi{3},sf2,2);

%columns filtering
y=D_1sf(lo,hi,sf1,1);
