% BRMLTOOLKIT
%
% Graph Theory
%   ancestors                - Return the ancestors of nodes x in DAG A
%   ancestralorder           - Return the ancestral order or the DAG A (oldest first)
%   descendents              - Return the descendents of nodes x in DAG A
%   children                 - return the children of variable x given adjacency matrix A
%   edges                    - Return edge list from adjacency matrix A
%   elimtri                  - Return a variable elimination sequence for a triangulated graph
%   connectedComponents      - Find the connected components of an adjacency matrix
%   istree                   - Check if graph is singly-connected 
%   neigh                    - Find the neighbours of vertex v on a graph with adjacency matrix G
%   noselfpath               - return a path excluding self transitions
%   parents                  - return the parents of variable x given adjacency matrix A
%   spantree                 - Find a spanning tree from an edge list
%   triangulate              - Triangulate adjacency matrix A
%   triangulatePorder        - Triangulate adjacency matrix A according to a partial ordering
%
%
% Potential manipulation
%   condpot                  - Return a potential conditioned on another variable
%   changevar                - Change variable names in a potential
%   dag                      - Return the adjacency matrix (zeros on diagonal) for a Belief Network
%   deltapot                 - A delta function potential 
%   disptable                - Print the table of a potential
%   divpots                  - Divide potential pota by potb
%   drawFG                   - Draw the Factor Graph A
%   drawID                   - plot an Influence Diagram
%   drawJTree                - plot a Junction Tree
%   drawNet                  - plot network
%   evalpot                  - Evaluate the table of a potential when variables are set
%   exppot                   - exponential of a potential
%   eyepot                   - Return a unit potential
%   grouppot                 - Form a potential based on grouping variables together
%   groupstate               - Find the state of the group variables corresponding to a given ungrouped state
%   logpot                   - logarithm of the potential
%   markov                   - Return a symmetric adjacency matrix of Markov Network in pot
%   maxpot                   - Maximise a potential over variables
%   maxsumpot                - Maximise or Sum a potential over variables
%   multpots                 - Multiply potentials into a single potential
%   numstates                - Number of states of the variables in a potential
%   orderpot                 - Return potential with variables reordered according to order
%   orderpotfields           - Order the fields of the potential, creating blank entries where necessary
%   potsample                - Draw sample from a single potential
%   potscontainingonly       - Returns those potential numbers that contain only the required variables 
%   potvariables             - Returns information about all variables in a set of potentials
%   setevpot                 - Sets variables in a potential into evidential states
%   setpot                   - sets potential variables to specified states
%   setstate                 - set a potential's specified joint state to a specified value
%   squeezepots              - Eliminate redundant potentials (those contained wholly within another)
%   sumpot                   - Sum potential pot over variables
%   sumpotID                 - Return the summed probability and utility tables from an ID
%   sumpots                  - Sum a set of potentials
%   table                    - Return the potential table
%   ungrouppot               - Form a potential based on ungrouping variables
%   ungroupstate             - Find the ungrouped states corresponding to a group state
%   uniquepots               - Eliminate redundant potentials (those contained wholly within another)
%   whichpot                 - Returns potentials that contain a set of variables%
%
% Routines also extend the toolbox to deal with Gaussian potentials:
% multpotsGaussianMoment.m, sumpotGaussianCanonical.m, sumpotGaussianMoment.m, multpotsGaussianCanonical.m
% See demoSumprodGaussCanon.m, demoSumprodGaussCanonLDS.m, demoSumprodGaussMoment.m 
%
%
% Inference
%   absorb                   - Update potentials in absorption message passing on a Junction Tree
%   absorption               - Perform full round of absorption on a Junction Tree
%   absorptionID             - Perform full round of absorption on an Influence Diagram
%   ancestralsample          - Ancestral sampling from a Belief Network
%   binaryMRFmap             - get the MAP assignment for a binary MRF with positive W
%   bucketelim               - Bucket Elimination on a set of potentials
%   condindep                - Conditional Independence check using graph of variable interactions
%   condindepEmp             - Compute the empirical log Bayes Factor and MI for independence/dependence
%   condindepPot             - Numerical conditional independence measure
%   condMI                   - conditional mutual information I(x,y|z) of a potential. 
%   FactorConnectingVariable - Factor nodes connecting to a set of variables
%   FactorGraph              - Returns a Factor Graph adjacency matrix based on potentials
%   IDvars                   - probability and decision variables from a partial order
%   jtassignpot              - Assign potentials to cliques in a Junction Tree
%   jtree                    - Setup a Junction Tree based on a set of potentials
%   jtreeID                  - Setup a Junction Tree based on an Influence Diagram
%   LoopyBP                  - loopy Belief Propagation using sum-product algorithm 
%   MaxFlow                  - MaxFlow Ford Fulkerson max flow - min cut algorithm (breadth first search)
%   maxNpot                  - Find the N most probable values and states in a potential
%   maxNprodFG               - N-Max-Product algorithm on a Factor Graph (Returns the Nmax most probable States)
%   maxprodFG                - maxprodFG Max-Product algorithm on a Factor Graph
%   MDPemDeterministicPolicy - Solve MDP using EM with deterministic policy
%   MDPsolve                 - MDPSOLVe solve a Markov Decision Process
%   MesstoFact               - Returns the message numbers that connect into factor potential
%   metropolis               - Metropolis sample
%   mostprobablepath         - Find the most probable path in a Markov Chain
%   mostprobablepathmult     - mostprobablepathmult Find the all source all sink most probable paths in a Markov Chain
%   sumprodFG                - Sum-Product algorithm on a Factor Graph represented by A
%
%
% Specific Models
%   ARlds                    - Learn AR coefficients using a Linear Dynamical System
%   ARtrain                  - Fit autoregressive (AR) coefficients of order L to v.
%   BayesLinReg              - Bayesian Linear Regression training using basis functions phi(x)
%   BayesLogRegressionRVM    - Bayesian Logistic Regression with the Relevance Vector Machine
%   CanonVar                 - Canonical Variates (no post rotation of variates)
%   cca                      - canonical correlation analysis
%   covfnGE                  - Gamma Exponential Covariance Function
%   EMbeliefnet              - train a Belief Network using Expectation Maximisation
%   EMminimizeKL             - MDP deterministic policy solver. Finds optimal actions
%   EMqTranMarginal          - EM marginal transition in MDP
%   EMqUtilMarginal          - Returns term proportional to the q marginal for the utility term
%   EMTotalBetaMessage       - backward information needed to solve the MDP process using message passing
%   EMvalueTable             - MDP solver calculates the value function of the MDP with the current policy
%   FA                       - Factor Analysis
%   GMMem                    - Fit a mixture of Gaussian to the data X using EM
%   GPclass                  - Gaussian Process Binary Classification
%   GPreg                    - Gaussian Process Regression
%   HebbML                   - Learn a sequence for a Hopfield Network
%   HMMbackward              - HMM Backward Pass
%   HMMbackwardSAR           - Backward Pass (beta method) for the Switching Autoregressive HMM
%   HMMem                    - EM algorithm for HMM
%   HMMforward               - HMM Forward Pass
%   HMMforwardSAR            - Switching Autoregressive HMM with switches updated only every Tskip timesteps
%   HMMgamma                 - HMM Posterior smoothing using the Rauch-Tung-Striebel correction method
%   HMMsmooth                - Smoothing for a Hidden Markov Model (HMM)
%   HMMsmoothSAR             - Switching Autoregressive HMM smoothing
%   HMMviterbi               - Viterbi most likely joint hidden state of a HMM
%   kernel                   - A kernel evaluated at two points
%   Kmeans                   - K-means clustering algorithm
%   LDSbackward              - Full Backward Pass for a Latent Linear Dynamical System (RTS correction method)
%   LDSbackwardUpdate        - Single Backward update for a Latent Linear Dynamical System (RTS smoothing update)
%   LDSforward               - Full Forward Pass for a Latent Linear Dynamical System (Kalman Filter)
%   LDSforwardUpdate         - Single Forward update for a Latent Linear Dynamical System (Kalman Filter)
%   LDSsmooth                - Linear Dynamical System : Filtering and Smoothing
%   LDSsubspace              - Subspace Method for identifying Linear Dynamical System
%   LogReg                   - Learning Logistic Linear Regression Using Gradient Ascent (BATCH VERSION)
%   MIXprodBern              - EM training of a Mixture of a product of Bernoulli distributions
%   mixMarkov                - EM training for a mixture of Markov Models
%   NaiveBayesDirichletTest  - Naive Bayes prediction having used a Dirichlet prior for training
%   NaiveBayesDirichletTrain - Naive Bayes training using a Dirichlet prior
%   NaiveBayesTest           - Test Naive Bayes Bernoulli Distribution after Max Likelihood training
%   NaiveBayesTrain          - Train Naive Bayes Bernoulli Distribution using Max Likelihood
%   nearNeigh                - Nearest Neighbour classification
%   pca                      - Principal Components Analysis
%   plsa                     - Probabilistic Latent Semantic Analysis
%   plsaCond                 - Conditional PLSA (Probabilstic Latent Semantic Analysis)
%   rbf                      - Radial Basis function output
%   SARlearn                 - EM training of a Switching AR model
%   SLDSbackward             - Backward pass using a Mixture of Gaussians
%   SLDSforward              - Switching Latent Linear Dynamical System Gaussian Sum forward pass
%   SLDSmargGauss            - compute the single Gaussian from a weighted SLDS mixture
%   softloss                 - Soft loss function
%   svdm                     - Singular Value Decomposition with missing values
%   SVMtrain                 - train a Support vector Machine
%
%
% General
%   argmax                   - performs argmax returning the index and value
%   assign                   - Assigns values to variables
%   betaXbiggerY             - p(x>y) for x~Beta(a,b), y~Beta(c,d)
%   bar3zcolor               - Plot a 3D bar plot of the matrix Z
%   avsigmaGauss             - Average of a logistic sigmoid under a Gaussian
%   cap                      - Cap x at absolute value c
%   chi2test                 - inverse of the chi square cumulative density
%   count                    - for a data matrix (each column is a datapoint), return the state counts
%   condexp                  - Compute p\propto exp(logp);
%   condp                    - Make a conditional distribution from the matrix
%   dirrnd                   - Samples from a Dirichlet distribution
%   field2cell               - Place the field of a structure in a cell
%   GaussCond                - Return the mean and covariance of a conditioned Gaussian
%   hinton                   - Plot a Hinton diagram
%   ind2subv                 - Subscript vector from linear index
%   lengthcell               - Length of each cell entry
%   logdet                   - Log determinant of a positive definite matrix computed in a numerically more stable manner
%   logeps                   - log(x+eps)
%   logGaussGamma            - unnormalised log of the Gauss-Gamma distribution
%   logsumexp                - Compute log(sum(exp(a).*b)) valid for large a
%   logZdirichlet            - Log Normalisation constant of a Dirichlet distribution with parameter u
%   majority                 - Return majority values in each column on a matrix
%   maxarray                 - Maximise a multi-dimensional array over a set of dimensions
%   maxNarray                - maxNarray Find the N highest values and states by maximising an array over a set of dimensions
%   mix2mix                  - Fit a mixture of Gaussians with another mixture of Gaussians
%   mvrandn                  - Samples from a multi-variate Normal(Gaussian) distribution
%   mygamrnd                 - Gamma random variate generator
%   mynanmean                - mean of values that are not nan 
%   mynansum                 - sum of values that are not nan
%   mynchoosek               - binomial coefficient v choose k
%   myones                   - same as ones(x), but if x is a scalar, interprets as ones([x 1])
%   myrand                   - same as rand(x) but if x is a scalar interprets as rand([x 1])
%   myzeros                  - same as zeros(x) but if x is a scalar interprets as zeros([x 1])
%   normp                    - Make a normalised distribution from an array
%   randgen                  - Generates discrete random variables given the pdf
%   replace                  - Replace instances of a value with another value
%   setdiff_unsorted         - 
%   sigma                    - 1./(1+exp(-x))
%   sigmoid                  - 1./(1+exp(-beta*x))
%   sqdist                   - Square distance between vectors in x and y
%   subv2ind                 - Linear index from subscript vector.
%   sumlog                   - sum(log(x)) with a cutoff at 10e-200
%
%
% Miscellaneous
%   compat                   - Compatibility of object F being in position h for image v on grid Gx,Gy
%   logp                     - The logarithm of a specific non-Gaussian distribution
%   placeobject              - Place the object F at position h in grid Gx,Gy
%   plotCov                  - return points for plotting an ellipse of a covariance
%   pointsCov                - Points defining the unit variance contours of a 2D Gaussian with mean m and covariance S
%   setup                    - run me at initialisation -- checks for bugs in matlab and initialises path 
%   validgridposition        - Returns 1 if point is on a defined grid
%   conjgrad                 - conjugate gradient solver for minimising a quadratic function
%   knapsackunbounded        - Unbounded knapsack solver
%   knapsackMultipleChoise   - Binary Multiple Choice knapsack solver
%   subsetsum                - zero subset sum solver