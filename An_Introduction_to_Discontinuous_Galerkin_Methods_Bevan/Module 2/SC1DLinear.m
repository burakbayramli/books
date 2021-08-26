pkg load signal

%Copyright (c) 2014, Josh Bevan
%All rights reserved.
%This code is licensed under the BSD 3-Clause License, see License.txt for the full license.

%--1D Scalar Conservation Eqn solution using Discontinuous Galerkin
%Linear spatial disretization, forward Euler time discretization
clear all
close all
tau=2*pi();

%--Here we attempt to solve the 1D scalar conservation eqn of the form:
%\frac{\partial u)(\partial t} + \frac{\partial f(u))(\partial x} = 0
%where f(u) is some flux function describing the "flow" of a conserved
%quantity. In this simplified case f(u) = u giving us a linear PDE
%We will use periodic boundary conditions to examine numerical dissipation
%effects

%--Let u(x,t) be the exact solution on the domain 0<=x<=1
%--Let u(.,0) = u0 = sin(2pi * x)

%--Let vh be the finite vector space of linear polynomials
%--Let uh be the approximate numerical solution consisting of a linear 
%combination of basis functions (\thetaN for the Nth basis) in vh with
%scalar coefficients BasisWeightN (i.e. \sum BasisWeightN \thetaN)
%--Let \phiN be the Nth test function in the same vector space as the basis
%functions (vh), in this simplified case N=2
N=2;

%--Discretize the domain into K elements with K+1 nodes
K=32;
xNode=0:1/K:1;
deltax = diff(xNode);
deltaxM = repmat(deltax,N,1);  %Repeat deltax for the N basis functions
%All the nodes, note their are repeats since nodes have a coincident
%brother from an adjacent element, except at the boundaries
xh = reshape(sort([xNode,xNode(2:end-1)]),N,K)';
u= sin(tau.*xh);


%--According to Cockburn,Shu 2001 Eqn 2.2 let uh(.,0) be computed by
%\int uh \phi = \int u0 \phi for each element (xj-1/2 < x < xj=1/2)
%--We can explicitly define a formula for the value of the RHS 
%ExactRHSN = \int u0 \phiN
ExactRHS(1,:)= (tau*deltax.*cos(tau.*xNode(1:end-1))+sin(tau.*xNode(1:end-1))-sin(tau.*xNode(2:end)))./(4*pi()^2.*(deltax));
ExactRHS(2,:)= -(tau*deltax.*cos(tau.*xNode(2:end))+sin(tau.*xNode(1:end-1))-sin(tau.*xNode(2:end)))./(4*pi()^2.*(deltax));

%--The LHS requires calculation of \int uh \phi
%which is \int \sum(aN \thetaN) \phi

%Matrix of LHS integrals of form \int \thetaI \phiJ, where \thetaI=\phiI
LHSIntegrals = [1/3 1/6;
                1/6 1/3];
%--Calculate basis weights for approximate solution
%LHSIntegrals * BasisWeights = ExactRHS / deltax
BasisWeights = LHSIntegrals\(ExactRHS./deltaxM);

%--Now that we have u0 we can begin explicit time stepping (forward Euler)
%with the semi-discrete form of the PDE.
%--Because we have a linear flux function all the classic monotone flux
%schemes reduce to the simple upwind flux i.e. g(v-(x),g(v+(x))=v-(x)

%--Let's assemble our semi-discrete stencil, assuming the BasisWeight 
%vector is of the form [a1_j a2_j a1_j+1 a2_j+1 ... a1_K a2_K]
UpwindFlux = [1 0  0;
              0 0 -1];
RHSIntegrals = [0 -1/2 -1/2;
                0  1/2  1/2];
Stencil = LHSIntegrals\(UpwindFlux + RHSIntegrals);

%--Assemble semi-discrete system
SemiMatrix = zeros(N*K);
for i=1 : size(Stencil,1) : size(SemiMatrix,2)-(size(Stencil,1)+1)
    SemiMatrix(i:i-1+size(Stencil,1),i:i-1+size(Stencil,2))=Stencil;
end
SemiMatrix = circshift(SemiMatrix, [0 -1]); %Apply periodic BC
SemiMatrix(end+1-size(Stencil,1):end,end+1-size(Stencil,2):end)=Stencil;
SemiMatrix = SemiMatrix./deltax(1);

BasisWeights = reshape(BasisWeights,N*K,1);
cats = BasisWeights;

%--Discretize in time and plot
deltaT= .0001;
saveT = 0.01;
nsaveT = floor(saveT/deltaT);
endT = 3;
nT = floor(endT/deltaT);
NormFreq = 100;
saved = zeros((nT/nsaveT)+1,K*N);
i=0;
norm2 = [];
for t= 0:1:nT
    %BasisWeights_dt = SemiMatrix*BasisWeights;
    %BasisWeights = BasisWeights+(BasisWeights_dt.*deltaT);
    BasisWeights = (eye(N*K)+SemiMatrix*deltaT)*BasisWeights;
    if t/nsaveT==floor(t/nsaveT)
        if i/NormFreq == floor(i/NormFreq)
            norm2 = [norm2 sum(([u(1,1) u(:,2)']'-BasisWeights([1 2:2:end])).^2)];
        end
        i= i+1;
        saved(i,:)=BasisWeights;
    end
end

saved = reshape(saved',N,K,length(saved));
j=0;

h=figure;set(gcf, 'Color','white')
% nFrames = length(saved);
% vidObj = VideoWriter('1DConvDG.avi');
% vidObj.Quality = 100;
% vidObj.FrameRate = floor(nFrames/(2*endT));
% open(vidObj);

i=length(saved);
for i=1:length(saved)
    plot(xh',saved(:,:,i))
    axis([0 1 -1.5 1.5])
    if (i-1)/NormFreq == floor((i-1)/NormFreq)
        j=j+1;
    end
    text(1.02,0.1,'L2-Norm')
    text(1.02,0,num2str(norm2(j)));
    text(1.02,-0.2,'RMS')
    text(1.02,-.3,num2str(rms(reshape(saved(2,:,i),K,1))));
    text(1.02,1.1,'Time')
    text(1.02,1,num2str((i-1)*saveT));
    %writeVideo(vidObj, getframe(h));
    pause(.001)
end

%close(gcf)
%close(vidObj);
