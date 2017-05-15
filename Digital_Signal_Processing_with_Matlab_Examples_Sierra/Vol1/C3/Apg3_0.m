% Analog Circuit transient response
aci=tf([1],[1 1]);
step(aci)
xlabel('seconds'); ylabel('Vo');title('analytical solution');
