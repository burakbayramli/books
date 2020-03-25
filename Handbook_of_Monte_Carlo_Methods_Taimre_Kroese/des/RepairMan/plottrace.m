%Repairman/plottrace.m
figure(1)
subplot(2,1,1),
for i =1:length(rr)-1,
    line([tt(i),tt(i+1)],[(nrep - rr(i)),(nrep -rr(i))]);
    line([tt(i+1),tt(i+1)],[(nrep -rr(i)),(nrep -rr(i+1))]);
end
aa=axis;
axis([0,tt(end),aa(3),aa(4)]),xlabel('t'),ylabel('# busy repair men');
subplot(2,1,2),
for i =1:length(ff)-1,
    line([tt(i),tt(i+1)],[(nmach - ff(i)),(nmach - ff(i))]);
    line([tt(i+1),tt(i+1)],[(nmach - ff(i)),(nmach - ff(i+1))]);
end
aa=axis;
axis([0,tt(end),aa(3),aa(4)]),xlabel('t'),ylabel('# working machines');
