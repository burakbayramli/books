function delErrorSquare = getDelError(dely0, dely1, h)
if (dely0 * dely1 >= 0)
    delErrorSquare = h * (dely0 * dely0 + dely0 * dely1 + dely1 * dely1) / 3;
else
    r = dely0 / (dely0 - dely1);
    startError = dely0 * dely0 / 3 * r * h;
    endError = dely1 * dely1 / 3 * (1 - r) * h;
    delErrorSquare = startError + endError;
end
