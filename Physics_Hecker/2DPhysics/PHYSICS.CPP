/*----------------------------------------------------------------------------

2D Physics Test Program - a cheesy test harness for 2D physics

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

physics.cpp - This file implements the 2D physics simulator.

10/15/96 - checker

*/

#include <assert.h>

#include "physics.h"
#include "iface.h"

/*----------------------------------------------------------------------------

Globals

*/

// weird CodeWarrior bug with global initialization forces me to do this
#define WIDTH 400
#define HEIGHT 400

int WorldWidth = WIDTH;
int WorldHeight = HEIGHT;

simulation_world World(r(WIDTH),r(HEIGHT));

/*----------------------------------------------------------------------------

various forces you can add to the system

@todo need to figure out units here so these numbers mean something

*/

int WorldSpringActive = 0;		// spring goes from body 0: vertex 0 to origin
real const Kws = r(30);			// Hooke's spring constant
real const Kwd = r(5);			// damping constant
vector_2 WorldSpringAnchor(r(0),r(0));

int BodySpringActive = 0;		// spring goes from body 0 to body 1
real const Kbs = r(10);			// Hooke's spring constant
real const Kbd = r(5);			// damping constant
int Body0SpringVertexIndex = 2;
int Body1SpringVertexIndex = 0;

int GravityActive = 0;
vector_2 Gravity(r(0),r(-100));

int DampingActive = 0;
real const Kdl = r(2.5);		// linear damping factor
real const Kda = r(1400);		// angular damping factor

/*----------------------------------------------------------------------------

Run

*/

void Run( void )
{
	static real LastTime = GetTime();

	// use a fixed timestep until we implement a better integrator
	// real Time = GetTime();
	real Time = LastTime + r(0.02);

	World.Simulate(Time - LastTime);

	World.Render();

	LastTime = Time;
}

/*----------------------------------------------------------------------------

Toggles

*/

void ToggleWorldSpring( void )
{
	WorldSpringActive = WorldSpringActive ? 0 : 1;
}

void ToggleBodySpring( void )
{
	BodySpringActive = BodySpringActive ? 0 : 1;
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

void InitializeBody( rigid_body &Body, real Density, real Width, real Height,
					real CoefficientOfRestitution )
{				
	real const Mass = Density * Width * Height;

	Body.CoefficientOfRestitution = CoefficientOfRestitution;

	Body.Width = Width;
	Body.Height = Height;

	Body.OneOverMass = r(1) / Mass;

	// integrate over the body to find the moment of inertia

	Body.OneOverCMMomentOfInertia = r(1) / ((Mass / r(12)) *
		(Width * Width + Height * Height));

	// 0-out non-vector quantities
	Body.aConfigurations[0].Orientation = r(0);
	Body.aConfigurations[0].AngularVelocity = r(0);
	Body.aConfigurations[0].Torque = r(0);
}

/*----------------------------------------------------------------------------

simulation_world ctor

*/

simulation_world::simulation_world( real WorldWidth_, real WorldHeight_ ) :
	WorldWidth(WorldWidth_), WorldHeight(WorldHeight_),
	SourceConfigurationIndex(0), TargetConfigurationIndex(1)
{
	// initialize bodies
	real const Density = r(0.01);
	InitializeBody(aBodies[0],Density,r(40),r(20),r(1));
	InitializeBody(aBodies[1],Density,r(50),r(10),r(1));

	aBodies[0].aConfigurations[0].CMVelocity = vector_2(r(40),r(10));
	aBodies[0].aConfigurations[0].AngularVelocity = r(PI);

	// initialize walls
	aWalls[0].Normal = vector_2(r(0),r(-1));
	aWalls[0].c = r(WorldHeight/2 - 3);

	aWalls[1].Normal = vector_2(r(0),r(1));
	aWalls[1].c = r(WorldHeight/2 - 3);

	aWalls[2].Normal = vector_2(r(-1),r(0));
	aWalls[2].c = r(WorldWidth/2 - 3);

	aWalls[3].Normal = vector_2(r(1),r(0));
	aWalls[3].c = r(WorldWidth/2 - 3);

	aWalls[4].Normal = GetNormal(vector_2(r(0.5),r(1)));
	aWalls[4].c = r(WorldWidth/2);

	// generate the wall lines
	for(int Counter = 0;Counter < NumberOfWalls;Counter++)
	{
		wall &Wall = aWalls[Counter];

		// make a big line in the direction of the wall

		vector_2 PointOnWall = -Wall.c * Wall.Normal;
		vector_2 v = GetPerpendicular(Wall.Normal);
		real t0 = -REAL_MAX;
		real t1 = REAL_MAX;

		// now clip the line to the walls

		for(int WallIndex = 0;WallIndex < NumberOfWalls;WallIndex++)
		{
			if(WallIndex != Counter)
			{
				wall &ClipWall = aWalls[WallIndex];

				real Denominator = DotProduct(v,ClipWall.Normal);

				if(fabs(Denominator) > Epsilon)
				{
					// not coplanar

					real t = - (ClipWall.c +
								DotProduct(PointOnWall,ClipWall.Normal)) /
									Denominator;

					if(Denominator > r(0))
					{
						// the clip wall's clipping the t0 side of line
						if(t > t0)
						{
							t0 = t;
						}
					}
					else
					{
						// it's clipping the t1 side
						if(t < t1)
						{
							t1 = t;
						}
					}
				}
			}
		}

		// make sure we got clipped
		assert((t0 != -REAL_MAX) && (t1 != REAL_MAX));
		// but not completely clipped
		assert(t0 < t1);

		Wall.StartPoint = PointOnWall + t0 * v;
		Wall.EndPoint = PointOnWall + t1 * v;
	}

	// calculate initial box positions
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

#pragma warning(disable:4244)		// float -> int
void simulation_world::Render( void )
{
	int Counter;

	// draw walls

	for(Counter = 0;Counter < NumberOfWalls;Counter++)
	{
		wall &Wall = aWalls[Counter];

		Line(Wall.StartPoint.X,Wall.StartPoint.Y,
			Wall.EndPoint.X,Wall.EndPoint.Y);
	}

	// draw bodies

	for(Counter = 0;Counter < NumberOfBodies;Counter++)
	{
		rigid_body::configuration::bounding_box &Box =
			aBodies[Counter].aConfigurations[SourceConfigurationIndex].
			BoundingBox;

		for(int Edge = 0;Edge < 4;Edge++)
		{
			int Start = Edge;
			int End = (Edge + 1) % 4;

			Line(Box.aVertices[Start].X,Box.aVertices[Start].Y,
				Box.aVertices[End].X,Box.aVertices[End].Y);
		}
	}

	// draw springs

	if(WorldSpringActive)
	{
		rigid_body::configuration::bounding_box &Box =
			aBodies[0].aConfigurations[SourceConfigurationIndex].BoundingBox;

		Line(Box.aVertices[0].X,Box.aVertices[0].Y,
			WorldSpringAnchor.X,WorldSpringAnchor.Y);
	}

	if(BodySpringActive)
	{
		rigid_body::configuration::bounding_box &Box0 =
			aBodies[0].aConfigurations[SourceConfigurationIndex].BoundingBox;
		rigid_body::configuration::bounding_box &Box1 =
			aBodies[1].aConfigurations[SourceConfigurationIndex].BoundingBox;

		assert(NumberOfBodies >= 2);
		vector_2 P0 = Box0.aVertices[Body0SpringVertexIndex];
		vector_2 P1 = Box1.aVertices[Body1SpringVertexIndex];
		Line(P0.X,P0.Y,P1.X,P1.Y);
	}

	// draw gravity vector

	if(GravityActive)
	{
		Line(0,0,0,-30);
		Line(0,-30,5,-25);
		Line(0,-30,-5,-25);
	}

	// draw damping symbol

	if(DampingActive)
	{
		Line(-5,0,5,0);
		Line(0,-5,0,5);
	}
}
#pragma warning(default:4244)		// float -> int

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

		Configuration.Torque = r(0);
		Configuration.CMForce = vector_2(r(0),r(0));

		if(GravityActive)
		{
			Configuration.CMForce += Gravity / Body.OneOverMass; 
		}

		if(DampingActive)
		{
			Configuration.CMForce += -Kdl * Configuration.CMVelocity;
			Configuration.Torque += -Kda * Configuration.AngularVelocity;
		}
	}

	if(BodySpringActive)
	{
		rigid_body &Body0 = aBodies[0];
		rigid_body::configuration &Configuration0 =
				Body0.aConfigurations[ConfigurationIndex];
		rigid_body::configuration::bounding_box &Box0 =
				Configuration0.BoundingBox;
		vector_2 Position0 = Box0.aVertices[Body0SpringVertexIndex];
		vector_2 U0 = Position0 - Configuration0.CMPosition;
		vector_2 VU0 = Configuration0.CMVelocity +
						Configuration0.AngularVelocity * GetPerpendicular(U0);

		rigid_body &Body1 = aBodies[1];
		rigid_body::configuration &Configuration1 =
				Body1.aConfigurations[ConfigurationIndex];
		rigid_body::configuration::bounding_box &Box1 =
				Configuration1.BoundingBox;
		vector_2 Position1 = Box1.aVertices[Body1SpringVertexIndex];
		vector_2 U1 = Position1 - Configuration1.CMPosition;
		vector_2 VU1 = Configuration1.CMVelocity +
						Configuration1.AngularVelocity * GetPerpendicular(U1);

		// spring goes from 0 to 1

		vector_2 SpringVector = Position1 - Position0;
		vector_2 Spring = -Kbs * SpringVector;

		vector_2 RelativeVelocity = VU1 - VU0;
		// project velocity onto spring to get damping vector
		// this is basically a Gram-Schmidt projection
		vector_2 DampingForce =
			-Kbd * (DotProduct(RelativeVelocity,SpringVector)/
					DotProduct(SpringVector,SpringVector)) * SpringVector;

		Spring += DampingForce;

		Configuration0.CMForce -= Spring;
		Configuration0.Torque -= PerpDotProduct(U0,Spring);

		Configuration1.CMForce += Spring;
		Configuration1.Torque += PerpDotProduct(U1,Spring);
	}

	if(WorldSpringActive)
	{
		// apply spring to body 0's vertex 0 to anchor

		rigid_body &Body = aBodies[0];
		rigid_body::configuration &Configuration =
				Body.aConfigurations[ConfigurationIndex];
		rigid_body::configuration::bounding_box &Box =
				Configuration.BoundingBox;

		vector_2 Position = Box.aVertices[0];

		vector_2 U = Position - Configuration.CMPosition;
		vector_2 VU = Configuration.CMVelocity +
						Configuration.AngularVelocity * GetPerpendicular(U);

		vector_2 Spring = -Kws * (Position - WorldSpringAnchor);
		// project velocity onto spring to get damping vector
		// this is basically a Gram-Schmidt projection
		vector_2 DampingForce =
			-Kwd * (DotProduct(VU,Spring)/DotProduct(Spring,Spring)) * Spring;

		Spring += DampingForce;

		Configuration.CMForce += Spring;
		Configuration.Torque += PerpDotProduct(U,Spring);
	}
}	


/*----------------------------------------------------------------------------

CalculateVertices - figure out the body vertices from the configuration

@todo should't this be in the body?

*/

void simulation_world::CalculateVertices( int ConfigurationIndex )
{
	int Counter;

	for(Counter = 0;Counter < NumberOfBodies;Counter++)
	{
		matrix_2x2 const Rotation(
			aBodies[Counter].aConfigurations[ConfigurationIndex].
				Orientation);

		vector_2 const Position =
			aBodies[Counter].aConfigurations[ConfigurationIndex].
				CMPosition;

		rigid_body::configuration::bounding_box &Box =
			aBodies[Counter].aConfigurations[ConfigurationIndex].BoundingBox;

		real const Width = aBodies[Counter].Width / 2.0f;
		real const Height = aBodies[Counter].Height / 2.0f;

		Box.aVertices[0] = Position + Rotation * vector_2( Width, Height);
		Box.aVertices[1] = Position + Rotation * vector_2( Width,-Height);
		Box.aVertices[2] = Position + Rotation * vector_2(-Width,-Height);
		Box.aVertices[3] = Position + Rotation * vector_2(-Width, Height);
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

		Target.CMPosition = Source.CMPosition +
				DeltaTime * Source.CMVelocity;

		Target.Orientation = Source.Orientation +
				DeltaTime * Source.AngularVelocity;

		Target.CMVelocity = Source.CMVelocity +
				(DeltaTime * aBodies[Counter].OneOverMass) * Source.CMForce;

		Target.AngularVelocity = Source.AngularVelocity +
				(DeltaTime * aBodies[Counter].OneOverCMMomentOfInertia) *
					Source.Torque;
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
	float const DepthEpsilon = 1.0f;

	real const HalfWidth = WorldWidth / 2.0f;
	real const HalfHeight = WorldHeight / 2.0f;

	for(int Body = 0;(Body < NumberOfBodies) &&
		(CollisionState != Penetrating);Body++)
	{
		// @todo active configuration number?!?!?
		rigid_body::configuration &Configuration =
				aBodies[Body].aConfigurations[ConfigurationIndex];

		rigid_body::configuration::bounding_box &Box =
				Configuration.BoundingBox;
	
		for(int Counter = 0;(Counter < 4) &&
			(CollisionState != Penetrating);Counter++)
		{
			vector_2 Position = Box.aVertices[Counter];

			vector_2 CMToCornerPerp =
				GetPerpendicular(Position - Configuration.CMPosition);

			vector_2 Velocity = Configuration.CMVelocity +
				Configuration.AngularVelocity * CMToCornerPerp;

			for(int WallIndex = 0;(WallIndex < NumberOfWalls) &&
				(CollisionState != Penetrating);WallIndex++)
			{
				wall &Wall = aWalls[WallIndex];

				real axbyc = DotProduct(Position,Wall.Normal) + Wall.c;

				if(axbyc < -DepthEpsilon)
				{
					CollisionState = Penetrating;
				}
				else
				if(axbyc < DepthEpsilon)
				{
					real RelativeVelocity = DotProduct(Wall.Normal,Velocity);

					if(RelativeVelocity < r(0))
					{
						CollisionState = Colliding;
						CollisionNormal = Wall.Normal;
						CollidingCornerIndex = Counter;
						CollidingBodyIndex = Body;
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

	vector_2 Position =
					Configuration.BoundingBox.aVertices[CollidingCornerIndex];

	vector_2 CMToCornerPerp = GetPerpendicular(Position -
					Configuration.CMPosition);

	vector_2 Velocity = Configuration.CMVelocity +
					Configuration.AngularVelocity * CMToCornerPerp;

	real ImpulseNumerator = -(r(1) + Body.CoefficientOfRestitution) *
					DotProduct(Velocity,CollisionNormal);

	float PerpDot = DotProduct(CMToCornerPerp,CollisionNormal);

	real ImpulseDenominator = Body.OneOverMass +
			Body.OneOverCMMomentOfInertia * PerpDot * PerpDot;

	real Impulse = ImpulseNumerator / ImpulseDenominator;

	Configuration.CMVelocity += Impulse * Body.OneOverMass * CollisionNormal;

	Configuration.AngularVelocity +=
							Impulse * Body.OneOverCMMomentOfInertia * PerpDot;
}

