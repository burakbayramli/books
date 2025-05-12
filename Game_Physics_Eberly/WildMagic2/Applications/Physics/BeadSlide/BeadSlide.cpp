// Magic Software, Inc.
// http://www.magic-software.com
// http://www.wild-magic.com
// Copyright (c) 2003.  All Rights Reserved
//
// The Game Physics source code is supplied under the terms of the license
// agreement http://www.magic-software.com/License/GamePhysics.pdf and may not
// be copied or disclosed except in accordance with the terms of that
// agreement.

#include "PhysicsModule.h"
#include <fstream>
using namespace std;

//----------------------------------------------------------------------------
void Simulation ()
{
    // set up the physics module
    PhysicsModule kModule;
    kModule.Gravity = 1.0;
    kModule.Mass = 0.1;

    double dTime = 0.0;
    double dDeltaTime = 0.001;
    double dQ = 1.0;
    double dQDer = 0.0;
    kModule.Initialize(dTime,dDeltaTime,dQ,dQDer);

    // run the simulation
    ofstream kOStr("simulation.txt");
    kOStr << "time  q  qder  position" << endl;
    const int iMax = 2500;
    for (int i = 0; i < iMax; i++)
    {
        double dX = dQ, dY = dQ*dQ, dZ = dQ*dY;

        char acMsg[512];
        sprintf(acMsg,"%5.3lf %+16.8lf %+16.8lf %+8.4lf %+8.4lf %+8.4lf",
            dTime,dQ,dQDer,dX,dY,dZ);
        kOStr << acMsg << endl;

        kModule.Update();

        dTime = kModule.GetTime();
        dQ = kModule.GetQ();
        dQDer = kModule.GetQDer();
    }
}
//----------------------------------------------------------------------------
int main ()
{
    Simulation();
    return 0;
}
//----------------------------------------------------------------------------
