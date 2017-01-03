function gain_plot(b,a,dBrange)
%gain_plot(b,a,dBrange)
%
%Produces a plot of gain in dB versus frequency in Hz-s
%
%inputs:  b,a =numerator, denominator weights of transfer function
%     dBrange =range from maximum to minimum gain in dB
%
%outputs: this function produces only a plot (fig. 10)

[H,v]=gain(b,a,1000);                   %complex gain
Hmax=max(abs(H));                       %maximum of |H|            
if Hmax==0
    error('Maximim gain is zero.')
end
Hmin1=Hmax*10^(-dBrange/20);            %minimum for plot
dB=20*log10(max(Hmin1,abs(H)));

sp_fig(10)
h=plot(v,dB,'-r'); grid;
set(h,'linewidth',2)
dBmax=max(dB)+.05*dBrange;
dBmin=10*floor(min(dB)/10);
set(gca,'fontsize',14,'ylim',[dBmin,dBmax])
xlabel('frequency in Hz-s)'); ylabel('gain in dB');
