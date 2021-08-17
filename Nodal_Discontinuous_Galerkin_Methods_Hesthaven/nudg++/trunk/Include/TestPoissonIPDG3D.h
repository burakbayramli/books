// TestPoissonIPDG3D.h
// 
// 2007/08/28
//---------------------------------------------------------
#ifndef NDG__TestPoissonIPDG3D_H__INCLUDED
#define NDG__TestPoissonIPDG3D_H__INCLUDED

#include "NDG3D.h"

//---------------------------------------------------------
class TestPoissonIPDG3D : public NDG3D
//---------------------------------------------------------
{
public:
  TestPoissonIPDG3D() { class_name = "TestPoissonIPDG3D"; }
  virtual ~TestPoissonIPDG3D() {}
  virtual void Driver();

protected:

  virtual void Run();

//virtual void Resize();
//virtual void SetIC();
//virtual void InitRun();
//virtual void Summary();
//virtual void Report(bool bForce=false);

protected:

//void BuildOps();
//void BuildPrecond();
//void BC();


  //-------------------------------------
  // member data
  //-------------------------------------


};

#endif  // NDG__TestPoissonIPDG3D_H__INCLUDED
