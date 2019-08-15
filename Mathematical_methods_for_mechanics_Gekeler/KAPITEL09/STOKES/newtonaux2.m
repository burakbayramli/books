function Y = newtonaux2(X,FLAG,RD)
% same as newtonaux1.m but with numerically calculated gradient
% very, very slow !!
% flag = 1: Funktion
% flag = 2: Gradient
switch FLAG
case 1
   Y = residuum(X);
case 2
   Y1 = residuum(X);
   n = length(X); epsilon = 1e-5  ;
   gradient = zeros(n,n);
   for i = 1:n
      if ~ismember(i,RD)
         aux = zeros(n,1); aux(i) = epsilon;
         X1 = X + aux;
         Y2 = residuum(X1);
         gradient(:,i) = (Y2 - Y1)/epsilon;  
      end
   end
   gradient(RD,:) = 0; 
   AUX = zeros(size(gradient,2),1); AUX(RD) = 1;
   gradient  = spdiags(diag(gradient)  + AUX,0,gradient);
   Y = gradient;
end
