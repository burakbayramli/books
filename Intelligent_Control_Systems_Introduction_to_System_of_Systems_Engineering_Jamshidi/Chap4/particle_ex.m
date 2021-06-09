%Particle filter example
%Written by Thrishantha Nanayakkara for the book on Intelligent Systems
%with an Introduction to System of Systems

a = -4; b = 4; %ranges of the state
x = a + (b-a).*rand(100,1);
figure(1);
hist(x,[a:0.4:b]);
xlabel('Value of state');
ylabel('frequency');

z = 0.3; % sensor reading
w = exp(-2*(x-0.3).^2).*exp(-3*(x-0.3^2).^2);
w1 = w/sum(w);
cw = cumsum(w1);
figure(2);
plot(cw,'-','linewidth',3);
xlabel('Index of the state');
ylabel('cumulative vale of the importance');

rx = [];
for i = 1:length(x)
rr = min(cw) +(max(cw) - min(cw))*rand;
diff = abs(cw - rr);
index = find(diff == min(diff));
rx = [rx;x(index)];
end

figure(3);
hist(rx,[a:0.4:b]);
xlabel('Value of state');
ylabel('frequency');
