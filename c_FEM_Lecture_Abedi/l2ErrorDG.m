function error = l2ErrorDG(xs, yExact, yNumerical)
dely = yExact - yNumerical;
numPts = length(dely);
numE = numPts / 2;
error = 0;
for i = 1:numE
    ptStart = 2 * i - 1;
    ptEnd = 2 * i;
    xStart = xs(ptStart);
    xEnd = xs(ptEnd);
    delyStart = dely(ptStart);
    delyEnd = dely(ptEnd);
    h = xEnd - xStart;
    delErrorSquare = getDelError(delyStart, delyEnd, h);
    error = error + delErrorSquare;
end
error = sqrt(error);