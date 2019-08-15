function XX = start2(X,STEP)
NX     = length(X); XX = X(:);
STEP1  = STEP*(sqrt(NX + 1) + NX - 1)/(NX*sqrt(2));
STEP2  = STEP*(sqrt(NX + 1) - 1)/(NX*sqrt(2));
A      = STEP2*ones(NX,NX+1);
A(:,1) = zeros(NX,1);
for I = 2:NX+1
   A(I-1,I) = STEP1;
end
B = A.';
CENTER = sum(B,1)/(NX+1); CENTER = CENTER(:);
XX = A + (XX - CENTER)*ones(1,NX+1);
%XX = A + XX*ones(1,NX+1);

