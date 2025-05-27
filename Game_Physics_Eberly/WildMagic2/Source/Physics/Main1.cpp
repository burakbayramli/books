#include "WmlRigidBody.h"
#include <iostream>

using namespace std;

int main() {
    cout << "Hello World!";
    double t = 0;
    double dt = 0.01;
    
    Wml::Vector3f x0 = Wml::Vector3f::ZERO;    

    Wml::Quaternionf q0{ 4.0f, 3.0f, 2.0f, 1.0f };

    Wml::Vector3f linMom0 = Wml::Vector3f::ZERO;    
    
    Wml::Vector3f angMom0 = Wml::Vector3f::ZERO;

    Wml::Vector3f extForce0 = Wml::Vector3f(3.0f,3.0f,3.0f);

    Wml::RigidBody<float> rb;

    rb.SetPosition(x0);

    rb.SetQOrientation(q0);

    rb.SetLinearMomentum(linMom0);

    rb.SetAngularMomentum(angMom0);

    rb.SetExternalForce(extForce0);
    
    for (int i = 0; i < 10; ++i)
    {
	if (i==0) rb.SetExternalForce(extForce0);
	rb.Update(t, dt);
	t += dt;
    }
    
    return 0;
}
