function D=DIVDIF(X,F)

%function D=DIVDIF(X,F)
%
%Berechnung der Koeffizienten des Interpolationspolynoms in Newtonscher Form
%
%Input:  X ... Vektor der Laenge n (Stuetzstellen), wobei die Elemente der
%              Groesse nach geordnet sein sollen
%        F ... Vektor der Laenge n, der die zu X gehoerenden Daten enthaelt,
%              die interpoliert werden sollen (Funktionswerte/Ableitungen)
%
%Output: D ... Vektor der Laenge n, der die Koeffizienten des Newtonschen
%              Interpolationspolynoms in aufsteigender Reihenfolge angibt
%
%written by Silke Schmid, Dezember 1995
%
%Dieses Programm dient zur Interpolation von Funktionswerten oder Ableitungen
%(geg. in F) an gegebenen Stuetzstellen X. Dabei wird die Rekursion der
%dividierten Differenzen benutzt, die letztendlich zu den Koeffizienten des
%Newtonschen Interpolationspolynoms fuehrt (vgl. Vektor D).
%Das Prinzip der Berechnung sieht folgendermassen aus. Zunaechst muss unter-
%schieden werden, an welchen Stellen a) Ableitungen und an welchen b) Funk-
%tionswerte interpoliert werden. Die entsprechenden Rechenregeln lauten:
%
%a) f[X(1),...,X(m)]=f^(m)(y)/m!, falls X(1)=...=X(m)=:y  (1<=m<=n)
%
%b) f[X(1),...,X(m)]=(f[X(2),...,X(m)]-f[X(1),...,X(m-1)])./(X(m)-X(1)), falls
% X(1)~=X(m) (1<=m<=n)
%
%Der Vektor D setzt sich dann aus den folgenden dividierten Differenzen zusam-
%men: D=[f[X(1)], f[X(1),X(2)],...,f[X(1),...,X(n)]].


n=length(X);
JJ=find((X([2:n])-X([1:n-1]))==0)+1;
D=F;

for m=1:length(JJ)
  F(JJ(m))=F(JJ(m)-1);
end

for j=2:n
  IND=[j:n];
  JJ=find((X(IND)-X(IND-j+1))==0)+j-1;
   K=find((X(IND)-X(IND-j+1))~=0)+j-1;
   F(K)=(F(K)-F(K-1))./(X(K)-X(K-j+1));

   if length(JJ)>0
     F(JJ(1))=D(JJ(1))/prod(1:j-1);

     for m=2:length(JJ)

       if JJ(m)==JJ(m-1)+1
         F(JJ(m))=F(JJ(m-1));
       else
         F(JJ(m))=D(JJ(m))/prod(1:j-1);
       end
     end
   end
end

D=F;








