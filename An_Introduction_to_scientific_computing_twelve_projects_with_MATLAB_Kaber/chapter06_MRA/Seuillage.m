function [DJ,somme]=Seuillage(J,CJ,epsilon)
somme=0;
DJ=CJ;
N=2^J;
for lin=1:N;
    for col=1:N
        if abs(DJ(lin,col))<=epsilon
            DJ(lin,col)=0;
        else
            somme=somme+1;
        end
    end
end
