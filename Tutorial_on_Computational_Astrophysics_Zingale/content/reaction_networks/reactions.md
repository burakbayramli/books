Nuclear Reactions
=================

Reaction Rates
--------------

Nuclear experiments provide us with reaction rates of the form (for a 2-body reaction):

$$r = \frac{n_A n_B}{1 + \delta_{AB}} \langle \sigma v \rangle$$

where $n_A$ is the number density of species A and $n_B$ is the number
density of species B.  The denominator ensures that we don't double
count for a species reacting with itself.  The term $\langle \sigma v
\rangle$ is the average of the cross section and velocity over the
distribution of velocities in the frame of the reaction.  This is what is
measured by experiements.

Here the rate, $r$, has units of reactions / time / volume.


Reaction databases (like ReacLib) often provide fits to $N_A \langle \sigma v
\rangle$, where $N_A$ is Avogadro's number.

Structure of a Network
----------------------

A reaction network is a set of nuclei and the rates that link them together.  Consider
4 nuclei, $A$, $B$, $C$, and $D$, linked by 2 rates:

$$A + A \rightarrow B + \gamma$$

$$B + C \rightarrow A + D$$

we would describe these as a set of ODEs for each species.  In terms
of number density, it is straightforward to write down:

\begin{eqnarray*}
\frac{dn_A}{dt} &=& - 2 \frac{1}{2} n_A^2 \langle \sigma v \rangle_{AA}
                     + n_B n_C \langle \sigma v \rangle_{BC} \\
\frac{dn_B}{dt} &=& \frac{1}{2} n_A^2 \langle \sigma v \rangle_{AA} 
                     - n_B n_C \langle \sigma v \rangle_{BC} \\
\frac{dn_C}{dt} &=& - n_B n_C \langle \sigma v \rangle_{BC} \\
\frac{dn_D}{dt} &=&  n_B n_C \langle \sigma v \rangle_{BC} \\
\end{eqnarray*}

Here the first equation says that we lose 2 nuclei $A$ for each $A + A$ reaction
and we gain 1 nuclei $A$ for each $B + C$ reaction.  The factor of 1/2 in the first
term is because when $A$ reacts with itself, we don't want to double count the number
of pairs.

We can instead write this in terms of molar or mass fractions.  Mass fractions are defined
as the mass of the species relative to the total mass of all species in a volume, or

$$X_k = \frac{\rho_k}{\rho}$$

These have the property

$$\sum_k X_k = 1$$

Molar fractions are scaled by the atomic weight:

$$Y_k = \frac{X_k}{A_k}$$

where $Y_k$ is the molar fraction of species $k$, $X_k$ is the mass fraction, and $A_k$
is the atomic weight.  Number density is related to mass fraction as:

$$n_k = \frac{\rho X_k}{m_u A_k}$$

where $m_u$ is the atomic mass unit ($1/N_A$).

Substituting these into the above expression we get

\begin{eqnarray*}
\frac{dY_A}{dt} &=& - 2 \frac{1}{2} \rho Y_A^2 N_A \langle \sigma v \rangle_{AA}
                     + \rho Y_B Y_C N_A \langle \sigma v \rangle_{BC} \\
\frac{dY_B}{dt} &=& \frac{1}{2} \rho Y_A^2 N_A \langle \sigma v \rangle_{AA} 
                     - \rho Y_B Y_C N_A \langle \sigma v \rangle_{BC} \\
\frac{dY_C}{dt} &=& - \rho Y_B Y_C N_A \langle \sigma v \rangle_{BC} \\
\frac{dY_D}{dt} &=&  \rho Y_B Y_C N_A \langle \sigma v \rangle_{BC} \\
\end{eqnarray*}

This is often the form we write the system of ODEs in when we write a network.

Integrating the Network
-----------------------

We often need to integrate this system together with an energy
equation to capture the evolution of the temperature as reactions
progress, since the reaction rates are highly-temperature sensitive.

But even without an energy equation, this system is difficult to
integrate because there can be a wide range of timescales involved in
the reaction rates, which makes the system a *stiff* system of ODEs.
We need to use different methods from the explicit Runge-Kutta methods
we often use.

