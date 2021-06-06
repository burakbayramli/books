/*----------------------------------------------------------------------------

3D Physics Test Program - a cheesy test harness for 3D physics

by Chris Hecker for my Game Developer Magazine articles.  See my homepage
for more information.

NOTE: This is a hacked test program, not a nice example of Windows programming.
physics.cpp the only part of this you should look at!!!

This material is Copyright 1997 Chris Hecker, All Rights Reserved.
It's for you to read and learn from, not to put in your own articles
or books or on your website, etc.  Thank you.

Chris Hecker
checker@d6.com
http://www.d6.com/users/checker

*/

/*----------------------------------------------------------------------------

physics.cpp - This file implements the 3D physics simulator.

5/24/96 - checker

*/

#if 1       // windows

// it's nice that you have include windows.h to use the cross-platform OpenGL
#define WIN32_LEAN_AND_MEAN
#define WIN32_EXTRA_LEAN
#include <windows.h>
#include <gl/gl.h>
#include <gl/glu.h>

#else       // mac, and others?

#include <gl.h>
#include <glu.h>

#endif

#include <assert.h>
#include <stdlib.h>
#include <limits.h>
#include <time.h>

#include "physics.h"
#include "iface.h"

/*----------------------------------------------------------------------------

Globals

*/

real const WorldSize = r(8);
simulation_world *pWorld = 0;

/*----------------------------------------------------------------------------

various forces you can add to the system

@todo need to figure out units here so these numbers mean something

*/

int WorldSpringsActive = 1;
real const Kws = r(0.7);        // Hooke's spring constant
real const Kwd = r(0.1);        // damping constant
struct world_spring
{
    int unsigned BodyIndex;
    int unsigned VertexIndex;
    vector_3 Anchor;
    world_spring( int unsigned B, int unsigned V, vector_3 const &A ) :
            BodyIndex(B), VertexIndex(V), Anchor(A) {};
} aWorldSprings[2] =
{
    world_spring(0,2,vector_3(r(-1.5),r(0),r(0))),
    world_spring(5,2,vector_3(r(1.5),r(0),r(0))),
};
int unsigned const NumberOfWorldSprings =
    sizeof(aWorldSprings)/sizeof(aWorldSprings[0]);

int BodySpringsActive = 1;
real const Kbs = r(0.6);        // Hooke's spring constant
real const Kbd = r(0.1);        // damping constant
struct body_spring
{
    int unsigned Body0Index;
    int unsigned Body0VertexIndex;
    int unsigned Body1Index;
    int unsigned Body1VertexIndex;
} aBodySprings[5] =
{
    0,6, 1,2,
    1,6, 2,2,
    2,6, 3,2,
    3,7, 4,2,
    4,6, 5,0,
};
int unsigned const NumberOfBodySprings =
    sizeof(aBodySprings)/sizeof(aBodySprings[0]);

int GravityActive = 1;
vector_3 Gravity(r(0),r(0),r(-10));

int DampingActive = 0;
real const NoKdl = r(0.002);        // the no-damping linear damping factor
real const NoKda = r(0.001);       // the no-damping angular damping factor
real const Kdl = r(0.04);           // linear damping factor
real const Kda = r(0.01);           // angular damping factor

/*----------------------------------------------------------------------------
  
Init & Run

*/

void Initialize( void )
{
    srand(time(0));

    glClearColor(0.0f,0.0f,0.0f,0.0f);
    glEnable(GL_CULL_FACE);
    glEnable(GL_DEPTH_TEST);
    //glPolygonMode(GL_BACK,GL_LINE);
    glShadeModel(GL_FLAT);

    float aLightPosition[4] = { 0.0f, 0.0f, 1.0f, 0.0f };
    glLightfv(GL_LIGHT0,GL_POSITION,aLightPosition);
    glEnable(GL_LIGHT0);
    glEnable(GL_COLOR_MATERIAL);
    glColorMaterial(GL_FRONT,GL_AMBIENT_AND_DIFFUSE);

    glMatrixMode(GL_PROJECTION);
    glViewport(0,0,WindowWidth,WindowHeight);
    gluPerspective(45.0,WindowWidth/WindowHeight,0.1,100.0);

    glMatrixMode(GL_MODELVIEW);

    pWorld = new simulation_world(WorldSize,WorldSize,WorldSize);
    assert(pWorld);
}

void Run( void )
{
    assert(pWorld);

    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    static real LastTime = GetTime();

#define FIXEDTIMESTEP 0
    // our Euler integrator sucks
    real const MaxTimeStep = r(0.01);
    
#if FIXEDTIMESTEP
    real Time = LastTime + MaxTimeStep;
#else
    real Time = GetTime();
#endif

    while(LastTime < Time)
    {
        real DeltaTime = Time - LastTime;
        if(DeltaTime > MaxTimeStep)
        {
            DeltaTime = MaxTimeStep;
        }
        
        pWorld->Simulate(DeltaTime);
        LastTime += DeltaTime;
    }
    LastTime = Time;

    pWorld->Render();
}

void Uninitialize( void )
{
    delete pWorld;
}

/*----------------------------------------------------------------------------

Toggles

*/

void ToggleWorldSprings( void )
{
    WorldSpringsActive = WorldSpringsActive ? 0 : 1;
}

void ToggleBodySprings( void )
{
    BodySpringsActive = BodySpringsActive ? 0 : 1;
}

void ToggleGravity( void )
{
    GravityActive = GravityActive ? 0 : 1;
}

void ToggleDamping( void )
{
    DampingActive = DampingActive ? 0 : 1;
}

/*----------------------------------------------------------------------------

utilities

*/

real GenerateUnitRandomReal( void )
{
    real Random = ((real)rand())/((real)RAND_MAX);
    assert((Random >= r(0)) && (Random <= r(1)));
    return Random;
}
        
real GenerateReasonableRandomReal( void )
{
    return r(0.1) + r(0.2)*GenerateUnitRandomReal();
}

void simulation_world::InitializeBodies( void )
{
    for(int BodyIndex = 0;BodyIndex < NumberOfBodies;BodyIndex++)
    {
        // initialize rigid bodies by randomly generating boxes

        rigid_body &Body = aBodies[BodyIndex];

        // 1/2 the dimensions of the box
        real dX2 = GenerateReasonableRandomReal();
        real dY2 = GenerateReasonableRandomReal();
        real dZ2 = GenerateReasonableRandomReal();

        real Density = r(0.4);
        real Mass = r(8)*Density*dX2*dY2*dZ2;
        assert(Mass > r(0));

        Body.OneOverMass = r(1)/Mass;
        Body.InverseBodyInertiaTensor(0,0) = r(3)/(Mass*(dY2*dY2+dZ2*dZ2));
        Body.InverseBodyInertiaTensor(1,1) = r(3)/(Mass*(dX2*dX2+dZ2*dZ2));
        Body.InverseBodyInertiaTensor(2,2) = r(3)/(Mass*(dX2*dX2+dY2*dY2));

        Body.CoefficientOfRestitution = r(1);

        // Body.aConfigurations[0].CMPosition;

        // initialize geometric quantities
        // we'll use the body index+1 for the display list id

        glNewList(BodyIndex+1,GL_COMPILE);

            glColor3f(GenerateUnitRandomReal(),
                      GenerateUnitRandomReal(),
                      GenerateUnitRandomReal());

            glBegin(GL_QUADS);
                glNormal3f(-1.0F, 0.0F, 0.0F);
                glTexCoord2f( 0.0F, 1.0F); glVertex3f(-dX2,-dY2,-dZ2);
                glTexCoord2f( 0.0F, 0.0F); glVertex3f(-dX2,-dY2, dZ2);
                glTexCoord2f( 1.0F, 0.0F); glVertex3f(-dX2, dY2, dZ2);
                glTexCoord2f( 1.0F, 1.0F); glVertex3f(-dX2, dY2,-dZ2);
                
                glNormal3f( 1.0F, 0.0F, 0.0F);
                glTexCoord2f( 1.0F, 1.0F); glVertex3f( dX2, dY2, dZ2);
                glTexCoord2f( 0.0F, 1.0F); glVertex3f( dX2,-dY2, dZ2);
                glTexCoord2f( 0.0F, 0.0F); glVertex3f( dX2,-dY2,-dZ2);
                glTexCoord2f( 1.0F, 0.0F); glVertex3f( dX2, dY2,-dZ2);

                glNormal3f( 0.0F,-1.0F, 0.0F);
                glTexCoord2f( 0.0F, 1.0F); glVertex3f(-dX2,-dY2,-dZ2);
                glTexCoord2f( 0.0F, 0.0F); glVertex3f( dX2,-dY2,-dZ2);
                glTexCoord2f( 1.0F, 0.0F); glVertex3f( dX2,-dY2, dZ2);
                glTexCoord2f( 1.0F, 1.0F); glVertex3f(-dX2,-dY2, dZ2);
                
                glNormal3f( 0.0F, 1.0F, 0.0F);
                glTexCoord2f( 1.0F, 1.0F); glVertex3f( dX2, dY2, dZ2);
                glTexCoord2f( 0.0F, 1.0F); glVertex3f( dX2, dY2,-dZ2);
                glTexCoord2f( 0.0F, 0.0F); glVertex3f(-dX2, dY2,-dZ2);
                glTexCoord2f( 1.0F, 0.0F); glVertex3f(-dX2, dY2, dZ2);
                
                glNormal3f( 0.0F, 0.0F,-1.0F);
                glTexCoord2f( 0.0F, 1.0F); glVertex3f(-dX2,-dY2,-dZ2);
                glTexCoord2f( 0.0F, 0.0F); glVertex3f(-dX2, dY2,-dZ2);
                glTexCoord2f( 1.0F, 0.0F); glVertex3f( dX2, dY2,-dZ2);
                glTexCoord2f( 1.0F, 1.0F); glVertex3f( dX2,-dY2,-dZ2);
                
                glNormal3f( 0.0F, 0.0F, 1.0F);
                glTexCoord2f( 1.0F, 1.0F); glVertex3f( dX2, dY2, dZ2);
                glTexCoord2f( 0.0F, 1.0F); glVertex3f(-dX2, dY2, dZ2);
                glTexCoord2f( 0.0F, 0.0F); glVertex3f(-dX2,-dY2, dZ2);
                glTexCoord2f( 1.0F, 0.0F); glVertex3f( dX2,-dY2, dZ2);
            glEnd();
        glEndList();

        // generate the body-space bounding volume vertices

        assert(Body.MaxNumberOfBoundingVertices > 8);
        Body.NumberOfBoundingVertices = 8;
        Body.aBodyBoundingVertices[0] = vector_3( dX2, dY2, dZ2);
        Body.aBodyBoundingVertices[1] = vector_3( dX2, dY2,-dZ2);
        Body.aBodyBoundingVertices[2] = vector_3( dX2,-dY2, dZ2);
        Body.aBodyBoundingVertices[3] = vector_3( dX2,-dY2,-dZ2);
        Body.aBodyBoundingVertices[4] = vector_3(-dX2, dY2, dZ2);
        Body.aBodyBoundingVertices[5] = vector_3(-dX2, dY2,-dZ2);
        Body.aBodyBoundingVertices[6] = vector_3(-dX2,-dY2, dZ2);
        Body.aBodyBoundingVertices[7] = vector_3(-dX2,-dY2,-dZ2);
    }
}

/*----------------------------------------------------------------------------

simulation_world ctor

*/

simulation_world::simulation_world(
    real WorldXLength, real WorldYLength, real WorldZLength ) :
    SourceConfigurationIndex(0), TargetConfigurationIndex(1)
{
    InitializeBodies();

    // initialize walls
    aWalls[0].Normal = vector_3(r(0),r(-1),r(0));
    aWalls[0].d = r(WorldYLength/2);
    aWalls[1].Normal = vector_3(r(0),r(1),r(0));
    aWalls[1].d = r(WorldYLength/2);

    aWalls[2].Normal = vector_3(r(-1),r(0),r(0));
    aWalls[2].d = r(WorldXLength/2);
    aWalls[3].Normal = vector_3(r(1),r(0),r(0));
    aWalls[3].d = r(WorldXLength/2);

    aWalls[4].Normal = vector_3(r(0),r(0),r(-1));
    aWalls[4].d = r(WorldZLength/2);
    aWalls[5].Normal = vector_3(r(0),r(0),r(1));
    aWalls[5].d = r(WorldZLength/2);

    // calculate initial bounding volume positions
    CalculateVertices(0);
}

/*----------------------------------------------------------------------------

simulation_world dtor

*/

simulation_world::~simulation_world( void )
{

}

/*----------------------------------------------------------------------------

Render - render the source configurations

*/

void simulation_world::Render( void )
{
    int Counter;

    // draw bodies

    glEnable(GL_LIGHTING);
    for(Counter = 0;Counter < NumberOfBodies;Counter++)
    {
        matrix_3x3 &Orientation = aBodies[Counter].
                aConfigurations[SourceConfigurationIndex].Orientation;
        vector_3 &CMPosition = aBodies[Counter].
                aConfigurations[SourceConfigurationIndex].CMPosition;
        float aM[16];

        glPushMatrix();
            CreateOpenGLTransform(Orientation,CMPosition,aM);
            glMultMatrixf(aM);
            glCallList(Counter+1);
        glPopMatrix();
    }
    glDisable(GL_LIGHTING);

#if 0
    // draw the bounding volume points
    glBegin(GL_POINTS);
        glColor3f(1.0f,1.0f,1.0f);
        for(Counter = 0;Counter < NumberOfBodies;Counter++)
        {
            rigid_body::configuration &Configuration = 
                aBodies[Counter].aConfigurations[SourceConfigurationIndex];

            for(int unsigned i = 0;i < Body.NumberOfBoundingVertices;i++)
            {
                glVertex3fv(Configuration.aBoundingVertices[i].GetElements());
            }
        }
    glEnd();
#endif

    // draw springs

    if(WorldSpringsActive)
    {
        glBegin(GL_LINES);
            for(int i = 0;i < NumberOfWorldSprings;i++)
            {
                world_spring &Spring = aWorldSprings[i];
                rigid_body::configuration &Configuration =
                        aBodies[Spring.BodyIndex].
                        aConfigurations[SourceConfigurationIndex];
    
                glColor3f(1.0f,1.0f,1.0f);
                glVertex3fv(Configuration.aBoundingVertices[Spring.VertexIndex].GetElements());
                glVertex3fv(Spring.Anchor.GetElements());
            }
        glEnd();
    }

    if(BodySpringsActive)
    {
        glBegin(GL_LINES);
            for(int i = 0;i < NumberOfBodySprings;i++)
            {
                body_spring &Spring = aBodySprings[i];
                assert((Spring.Body0Index < NumberOfBodies) &&
                        (Spring.Body1Index < NumberOfBodies));
    
                rigid_body::configuration &Configuration0 =
                    aBodies[Spring.Body0Index].
                    aConfigurations[SourceConfigurationIndex];
                rigid_body::configuration &Configuration1 =
                    aBodies[Spring.Body1Index].
                    aConfigurations[SourceConfigurationIndex];
        
                glColor3f(1.0f,1.0f,1.0f);
                glVertex3fv(Configuration0.aBoundingVertices[
                            Spring.Body0VertexIndex].GetElements());
                glVertex3fv(Configuration1.aBoundingVertices[
                            Spring.Body1VertexIndex].GetElements());
            }
        glEnd();
    }

    // draw walls...I didn't bother implementing the cool clipper thing
    // like in the 2D sample
    assert(NumberOfWalls == 6);     // @todo sort of assert it's a cube

    glColor3f(1.0f,1.0f,1.0f);
    // do a big linestrip to get most of edges
    glBegin(GL_LINE_STRIP);
        glVertex3f(-WorldSize/r(2),-WorldSize/r(2),-WorldSize/r(2));
        glVertex3f(-WorldSize/r(2),-WorldSize/r(2), WorldSize/r(2));
        glVertex3f(-WorldSize/r(2), WorldSize/r(2), WorldSize/r(2));
        glVertex3f( WorldSize/r(2), WorldSize/r(2), WorldSize/r(2));
        glVertex3f( WorldSize/r(2),-WorldSize/r(2), WorldSize/r(2));
        glVertex3f( WorldSize/r(2),-WorldSize/r(2),-WorldSize/r(2));
    glEnd();
    // fill in the stragglers
    glBegin(GL_LINES);
        glVertex3f( WorldSize/r(2),-WorldSize/r(2), WorldSize/r(2));
        glVertex3f(-WorldSize/r(2),-WorldSize/r(2), WorldSize/r(2));

        glVertex3f( WorldSize/r(2), WorldSize/r(2), WorldSize/r(2));
        glVertex3f( WorldSize/r(2), WorldSize/r(2),-WorldSize/r(2));

        glVertex3f(-WorldSize/r(2), WorldSize/r(2), WorldSize/r(2));
        glVertex3f(-WorldSize/r(2), WorldSize/r(2),-WorldSize/r(2));
    glEnd();
    
    // draw floor
    glDisable(GL_CULL_FACE);
    glBegin(GL_QUADS);
        glColor3f(0.5f,0.5f,0.5f);
        glVertex3f( WorldSize/r(2), WorldSize/r(2),-WorldSize/r(2));
        glVertex3f(-WorldSize/r(2), WorldSize/r(2),-WorldSize/r(2));
        glVertex3f(-WorldSize/r(2),-WorldSize/r(2),-WorldSize/r(2));
        glVertex3f( WorldSize/r(2),-WorldSize/r(2),-WorldSize/r(2));
    glEnd();
    glEnable(GL_CULL_FACE);
}

/*----------------------------------------------------------------------------

Simulate - Integrate forward DeltaTime seconds.

@todo do I need to store the last simulation time?

*/

void simulation_world::Simulate( real DeltaTime )
{
    real CurrentTime = r(0);
    real TargetTime = DeltaTime;

    while(CurrentTime < DeltaTime)
    {
        ComputeForces(SourceConfigurationIndex);

        Integrate(TargetTime-CurrentTime);

        CalculateVertices(TargetConfigurationIndex);

        CheckForCollisions(TargetConfigurationIndex);

        if(CollisionState == Penetrating)
        {
            // we simulated too far, so subdivide time and try again
            TargetTime = (CurrentTime + TargetTime) / r(2);

            // blow up if we aren't moving forward each step, which is
            // probably caused by interpenetration at the frame start

            assert(fabs(TargetTime - CurrentTime) > Epsilon);
        }
        else
        {
            // either colliding or clear

            if(CollisionState == Colliding)
            {
                // @todo handle multiple simultaneous collisions

                int Counter = 0;
                do
                {
                    ResolveCollisions(TargetConfigurationIndex);
                    Counter++;
                } while((CheckForCollisions(TargetConfigurationIndex) ==
                            Colliding) && (Counter < 100));

                assert(Counter < 100);
            }

            // we made a successful step, so swap configurations
            // to "save" the data for the next step
            
            CurrentTime = TargetTime;
            TargetTime = DeltaTime;

            SourceConfigurationIndex = SourceConfigurationIndex ? 0 : 1;
            TargetConfigurationIndex = TargetConfigurationIndex ? 0 : 1;
        }
    }
}

/*----------------------------------------------------------------------------

ComputeForces - compute forces for gravity, spring, etc.

*/

void simulation_world::ComputeForces( int ConfigurationIndex )
{
    int Counter;

    for(Counter = 0;Counter < NumberOfBodies;Counter++)
    {
        rigid_body &Body = aBodies[Counter];
        rigid_body::configuration &Configuration =
            Body.aConfigurations[ConfigurationIndex];

        // clear forces

        Configuration.Torque = vector_3();
        Configuration.CMForce = vector_3();

        if(GravityActive)
        {
            Configuration.CMForce += Gravity / Body.OneOverMass; 
        }

        if(DampingActive)
        {
            Configuration.CMForce += -Kdl * Configuration.CMVelocity;
            Configuration.Torque += -Kda * Configuration.AngularVelocity;
        }
        else
        {
            // there's always a little damping because our integrator sucks
            Configuration.CMForce += -NoKdl * Configuration.CMVelocity;
            Configuration.Torque += -NoKda * Configuration.AngularVelocity;
        }
    }

    if(BodySpringsActive)
    {
        for(int i = 0;i < NumberOfBodySprings;i++)
        {
            body_spring &SpringStructure = aBodySprings[i];

            rigid_body &Body0 = aBodies[SpringStructure.Body0Index];
            rigid_body::configuration &Configuration0 =
                Body0.aConfigurations[ConfigurationIndex];
            vector_3 Position0 =
                Configuration0.aBoundingVertices[
                    SpringStructure.Body0VertexIndex];
            vector_3 U0 = Position0 - Configuration0.CMPosition;
            vector_3 VU0 = Configuration0.CMVelocity +
                CrossProduct(Configuration0.AngularVelocity,U0);
    
            rigid_body &Body1 = aBodies[SpringStructure.Body1Index];
            rigid_body::configuration &Configuration1 =
                Body1.aConfigurations[ConfigurationIndex];
            vector_3 Position1 =
                Configuration1.aBoundingVertices[
                    SpringStructure.Body1VertexIndex];
            vector_3 U1 = Position1 - Configuration1.CMPosition;
            vector_3 VU1 = Configuration1.CMVelocity +
                CrossProduct(Configuration1.AngularVelocity,U1);
    
            // spring goes from 0 to 1
    
            vector_3 SpringVector = Position1 - Position0;
            vector_3 Spring = -Kbs * SpringVector;
    
            vector_3 RelativeVelocity = VU1 - VU0;
            // project velocity onto spring to get damping vector
            // this is basically a Gram-Schmidt projection
            vector_3 DampingForce =
                -Kbd * (DotProduct(RelativeVelocity,SpringVector)/
                        DotProduct(SpringVector,SpringVector)) * SpringVector;
    
            Spring += DampingForce;
    
            Configuration0.CMForce -= Spring;
            Configuration0.Torque -= CrossProduct(U0,Spring);
    
            Configuration1.CMForce += Spring;
            Configuration1.Torque += CrossProduct(U1,Spring);
        }
    }

    if(WorldSpringsActive)
    {
        for(int i = 0;i < NumberOfWorldSprings;i++)
        {
            world_spring &SpringStructure = aWorldSprings[i];
            
            rigid_body &Body = aBodies[SpringStructure.BodyIndex];
            rigid_body::configuration &Configuration =
                Body.aConfigurations[ConfigurationIndex];
            
            vector_3 Position = Configuration.aBoundingVertices[
                SpringStructure.VertexIndex];
            vector_3 U = Position - Configuration.CMPosition;
            vector_3 VU = Configuration.CMVelocity +
                CrossProduct(Configuration.AngularVelocity,U);
            
            vector_3 Spring = -Kws * (Position - SpringStructure.Anchor);
            // project velocity onto spring to get damping vector
            // this is basically a Gram-Schmidt projection
            vector_3 DampingForce =
                -Kwd*(DotProduct(VU,Spring)/DotProduct(Spring,Spring)) * Spring;
            
            Spring += DampingForce;
            
            Configuration.CMForce += Spring;
            Configuration.Torque += CrossProduct(U,Spring);
        }
    }
}   


/*----------------------------------------------------------------------------

CalculateVertices - figure out the body vertices from the configuration

@todo should't this be in the body?

*/

void simulation_world::CalculateVertices( int ConfigurationIndex )
{
    for(int Counter = 0;Counter < NumberOfBodies;Counter++)
    {
        rigid_body &Body = aBodies[Counter];
        rigid_body::configuration &Configuration =
                Body.aConfigurations[ConfigurationIndex];
            
        matrix_3x3 const &A = Configuration.Orientation;
        vector_3 const &R = Configuration.CMPosition;

        assert(Body.NumberOfBoundingVertices <
               Body.MaxNumberOfBoundingVertices);
        for(int unsigned i = 0;i < Body.NumberOfBoundingVertices;i++)
        {
            Configuration.aBoundingVertices[i] =
                R + A * Body.aBodyBoundingVertices[i];
        }
    }
}

/*----------------------------------------------------------------------------

Integrate - integrate the rigid body configurations forward in time from
                source config to target config

@todo calculate initial derivatives _once_
@todo use something better than Euler's method

*/

void simulation_world::Integrate( real DeltaTime )
{
    int Counter;

    for(Counter = 0;Counter < NumberOfBodies;Counter++)
    {
        rigid_body::configuration &Source =
            aBodies[Counter].aConfigurations[SourceConfigurationIndex];
        rigid_body::configuration &Target =
            aBodies[Counter].aConfigurations[TargetConfigurationIndex];

        // integrate primary quantities

        Target.CMPosition = Source.CMPosition +
                DeltaTime * Source.CMVelocity;

        Target.Orientation = Source.Orientation +
                DeltaTime *
                matrix_3x3(Source.AngularVelocity,matrix_3x3::SkewSymmetric) *
                Source.Orientation;

        Target.CMVelocity = Source.CMVelocity +
                (DeltaTime * aBodies[Counter].OneOverMass) * Source.CMForce;

        Target.AngularMomentum = Source.AngularMomentum +
                DeltaTime * Source.Torque;

        OrthonormalizeOrientation(Target.Orientation);

        // compute auxiliary quantities

        Target.InverseWorldInertiaTensor = Target.Orientation *
                aBodies[Counter].InverseBodyInertiaTensor *
                Transpose(Target.Orientation);

        Target.AngularVelocity = Target.InverseWorldInertiaTensor *
                Target.AngularMomentum;
    }
}


/*----------------------------------------------------------------------------

CheckForCollisions - test the current configuration for collisions

@todo handle multiple simultaneous collisions

*/

simulation_world::collision_state
simulation_world::CheckForCollisions( int ConfigurationIndex )
{
    // be optimistic!
    CollisionState = Clear;
    float const DepthEpsilon = 0.001f;

    for(int BodyIndex = 0;(BodyIndex < NumberOfBodies) &&
            (CollisionState != Penetrating);BodyIndex++)
    {
        rigid_body &Body = aBodies[BodyIndex];
        rigid_body::configuration &Configuration =
                Body.aConfigurations[ConfigurationIndex];

        for(int unsigned Counter = 0;
            (Counter < Body.NumberOfBoundingVertices) &&
                (CollisionState != Penetrating);Counter++)
        {
            vector_3 Position = Configuration.aBoundingVertices[Counter];
            vector_3 U = Position - Configuration.CMPosition;

            vector_3 Velocity = Configuration.CMVelocity +
                        CrossProduct(Configuration.AngularVelocity,U);

            for(int WallIndex = 0;(WallIndex < NumberOfWalls) &&
                (CollisionState != Penetrating);WallIndex++)
            {
                wall &Wall = aWalls[WallIndex];

                real axbyczd = DotProduct(Position,Wall.Normal) + Wall.d;

                if(axbyczd < -DepthEpsilon)
                {
                    CollisionState = Penetrating;
                }
                else
                if(axbyczd < DepthEpsilon)
                {
                    real RelativeVelocity = DotProduct(Wall.Normal,Velocity);

                    if(RelativeVelocity < r(0))
                    {
                        CollisionState = Colliding;
                        CollisionNormal = Wall.Normal;
                        CollidingCornerIndex = Counter;
                        CollidingBodyIndex = BodyIndex;
                    }
                }
            }
        }
    }

    return CollisionState;
}


/*----------------------------------------------------------------------------

ResolveCollisions - calculate collision impulses

@todo handle multiple simultaneous collisions

*/

void simulation_world::ResolveCollisions( int ConfigurationIndex )
{
    rigid_body &Body = aBodies[CollidingBodyIndex];
    rigid_body::configuration &Configuration =
        Body.aConfigurations[ConfigurationIndex];
    
    vector_3 Position =
        Configuration.aBoundingVertices[CollidingCornerIndex];
    
    vector_3 R = Position - Configuration.CMPosition;

    vector_3 Velocity = Configuration.CMVelocity +
        CrossProduct(Configuration.AngularVelocity,R);
    
    real ImpulseNumerator = -(r(1) + Body.CoefficientOfRestitution) *
        DotProduct(Velocity,CollisionNormal);

    real ImpulseDenominator = Body.OneOverMass +
        DotProduct(CrossProduct(Configuration.InverseWorldInertiaTensor *
                                CrossProduct(R,CollisionNormal),R),
                   CollisionNormal);
    
    vector_3 Impulse = (ImpulseNumerator/ImpulseDenominator) * CollisionNormal;
    
    // apply impulse to primary quantities
    Configuration.CMVelocity += Body.OneOverMass * Impulse;
    Configuration.AngularMomentum += CrossProduct(R,Impulse);
    
    // compute affected auxiliary quantities
    Configuration.AngularVelocity = Configuration.InverseWorldInertiaTensor *
        Configuration.AngularMomentum;
}

