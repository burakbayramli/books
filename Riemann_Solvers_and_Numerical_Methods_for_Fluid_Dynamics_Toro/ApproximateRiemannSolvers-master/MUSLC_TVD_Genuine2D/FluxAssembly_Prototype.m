% Manual assembly of fluxes

    % Restrict certain crossings
    if (sNE < 0) && (sEN <0)        % Northeast
        if sWN > 0; sEN = 0; end
        if sSE > 0; sNE = 0; end
    end
    if (sNW < 0) && (sWN <0)        % Northwest
        if sSW > 0; sNW = 0; end
        if sEN > 0; sWN = 0; end
    end
    if (sSW < 0) && (sWS <0)        % Southwest
        if sNW > 0; sSW = 0; end
        if sES > 0; sWS = 0; end
    end
    if (sSE < 0) && (sES <0)        % Southeast
        if sNE > 0; sSE = 0; end
        if sWS > 0; sES = 0; end
    end

    % Define speeds \tilde{s}_alpha for alpha \in (N,S,E,W)
    if (sES>=0) && (sWS>=0)     % Above x-axis
        sE = sSE;
        sW = sSW;
    elseif (sEN<=0) && (sWN<=0) % Below x-axis
        sE = sNE;
        sW = sNW;
    else
        sE = max(sNE,0)-max(sEN,0)*(max(sSE,0)-max(sNE,0))/(min(sES,0)-max(sEN,0));
        sW = min(sSW,0)-min(sWS,0)*(min(sNW,0)-min(sSW,0))/(max(sWN,0)-min(sWS,0));
    end
    if (sNW>=0) && (sSW>=0)     % Right of y-axis
        sN = sWN;
        sS = sWS;
    elseif (sNE<=0) && (sSE<=0) % Left of y-axis
        sN = sEN;
        sS = sES;
    else
        sN = max(sWN,0)-min(sNW,0)*(max(sWN,0)-max(sEN,0))/(min(sNW,0)-max(sNE,0));
        sS = min(sES,0)-max(sSE,0)*(min(sES,0)-min(sWS,0))/(max(sSE,0)-min(sSW,0));
    end

    % Define fluxes phiN^{~HLL2D}, phiS^{~HLL2D}, phiE^{~HLL2D} and phiW^{~HLL2D}
    sY = max(abs(sN),abs(sS));
    sX = max(abs(sE),abs(sW));
    %
    if (sW>=0) && (sS>=0)
        fooN = ((sN-sS)*foW+sS*fSW)/sN;
        fooS = fSW;
        gooE = ((sE-sW)*gSo+sW*gSW)/sE;
        gooW = gSW;
    elseif (sW>=0) && (sN<=0)
        fooN = fNW;
        fooS = ((sS-sN)*foW+sN*fNW)/sS;
        gooE = ((sE-sW)*gSo+sW*gNW)/sE;
        gooW = gNW;
    elseif (sE<=0) && (sS>=0)
        fooN = ((sN-sS)*foE+sS*fSE)/sN;
        fooS = fSE;
        gooE = gSE;
        gooW = ((sW-sE)*gSo+sE*gSE)/sW;
    elseif (sE<=0) && (sN<=0)
        fooN = fNE;
        fooS = ((sS-sN)*foE+sN*fNE)/sS;
        gooE = gNE;
        gooW = ((sW-sE)*gNo+sE*gNE)/sW;
    elseif sW>=0
        fooN = ((sY+sN)*fNW-sN*foW)/sY;
        fooS = ((sY-sS)*fSW+sS*goW)/sY;
        gooE = ((sE-sW)*goo+sW*goW)/sE;
        gooW = goW;
    elseif sE<=0
        fooN = ((sY-sN)*fNE+sN*foE)/sY;
        fooS = ((sY+sN)*fSE-sS*foE)/sY;
        gooE = goE;
        gooW = ((sW-sE)*goo+sE*goE)/sW;
    elseif sS>=0
        fooN = ((sN-sS)*foo+sS*fSo)/sN;
        fooS = fSo;
        gooE = ((sX-sE)*gSE+sE*gSo)/sX;
        gooW = ((sX+sW)*gSW-sW*gSo)/sX;
    elseif sN<=0
        fooN = fNo;
        fooS = ((sS-sN)*foo+sN*fNo)/sS;
        gooE = ((sX-sE)*gNE+sE*gNo)/sX;
        gooW = ((sX+sW)*gNW-sW*gNo)/sX;
    else
        fooN = ((sY-sN)*fNo+sN*foo)/sY;
        fooS = ((sY+sS)*fSo-sS*foo)/sY;
        gooE = ((sX-sE)*goE+sE*goo)/sX;
        gooW = ((sX+sW)*goW-sW*goo)/sX;
    end