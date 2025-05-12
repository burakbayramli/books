CG Converter
============

Usage
-----
cgconverter <inputfile> [-quiet] [-entry <entry>] [-output <output>] [-forceVS2] -vertex
cgconverter <inputfile> [-quiet] [-entry <entry>] [-output <output>] -pixel


Options
-------
<inputfile>
	The filename of the Cg program to be converted.  Required.

-quiet
	Do not output anything.

-entry <entry>
	The "main" function of the shader.  Defaults to vmain for vertex shaders and pmain for pixel shaders.

-output <output>
	The output file.  Defaults to inputfile + ".wvs" for vertex shaders and inputfile + ".wps" for pixel shaders.

-vertex/-pixel
	This Cg file contains a vertex/pixel shader.  Exactly one is required.

-forceVS2
	*** Unfortunately, there is no way for the Cg runtime to be able to tell if your program is too long or not.  Thus, the CgConverter can't tell that even though your program compiles syntactically into vs_1_1, it won't run because of the instruction limit.  Pixel shaders do not have this problem.  So, if your program seems long (>128 instructions) and you get errors compiling in DirectX or cgc issues a warning that your program is too long, issue this flag to fix that problem.

Requirements
------------
Cg development environment.


General Usage
-------------
The CgConverter takes a Cg file and constructs a shader object that WildMagic can load easily.  It compiles the program into both DirectX and OpenGL capable versions.  For each of these, the program tries to pick the version which will run on the most graphics cards.  It stores these in the file.  The other important use of the CgConverter is that it reads through the program and looks at all the parameters, the results of which it outputs onto the command line.  The registers that varying and uniform input parameters are stored in are evaluated and stored.  State constants are also identified and stored in the file.  It stores all of this information about the parameters as a ShaderConstants object in the file as well.

One thing the Cg Converter does not do is give you detailed compile time error information.  If your program has errors, the converter will not succesfully run and will give you errors that it couldn't get the program to compile under any profiles.  However, it won't give you any more information than that.  It is recommended that in your project files you run the cgc compiler first (perhaps with a -nocode flag) before the cg converter.  That way when there are errors in the program, you can get better information about them.


Accessing Rendering State
-------------------------

You can access nearly all WildMagic rendering state from within both your vertex and pixel shader programs in both DirectX and OpenGL.  To access renderer state, you use certain variable names (which are all prefixed by "Wml").  Because of this, if you prefix a variable with "Wml" and it is not a valid state constant, the Cg Converter will throw an error.  The names should be fairly self explanatory about what state they represent.  Here is a listing of types and names from Source/WmlStateConstant.cpp: (See Source/WmlStateConstant.* for more details)

	float4 WmlCameraPosition
	float4 WmlCameraUp
	float4 WmlCameraLeft
	float4 WmlCameraDirection

	float4x4 WmlRendererModViewProj (*)
	float4x4 WmlRendererModView     (*) 
	float4x4 WmlRendererMod         (*)
	float4x4 WmlRendererProj        (*)

	float4 WmlFogColor
	float4 WmlFogParams

	float4 WmlMaterialEmissive
	float4 WmlMaterialAmbient
	float4 WmlMaterialDiffuse
	float4 WmlMaterialSpecular
	float4 WmlMaterialShininess

	float4 WmlLightPosition#        (**)
	float4 WmlLightDirection#       (**)
	float4 WmlLightAmbient#         (**)
	float4 WmlLightDiffuse#         (**)
	float4 WmlLightSpecular#        (**)
	float4 WmlLightSpotcutoff#      (**)
	float4 WmlLightAttenparams#     (**)

(*): All matrix state constants can optionally have certain suffixes: "Trans", "Inv", and "InvTrans".  These correspond to transformations (transpose, inverse, and inverse transpose respectively) on that matrix.  For example, WmlRendererModInv or WmlRendererProjInvTrans are both valid state constants.  As a sidenote, the "Mod" in the matrix state constants means the "Model->World" transform.

(**): All light constants are REQUIRED to have a number after them, corresponding to which light it refers to.  Thus, WmlLightPosition0 and WmlLightAmbient5 are valid state constants.  It is the responsibility of the user to ensure that the lights that are referenced have been defined and attached as state to the part of the scene graph that is using the shader by the application itself.


Application Use
---------------

WildMagic allows easy access to the uniform constants that you have created in the program.  First you create and attach your shader.  Shaders can only be attached to Geometry objects.

	Geometry* pkGeom;
	...
	// Set shader
	pkGeom->SetVertexShader( new VertexShader( "someshader.wvs" ) );
	pkGeom->SetPixelShader( new PixelShader( "someshader.wps" ) );
	// Set constants
	...

If you want to detach a shader:
	
	pkGeom->SetVertexShader(NULL);
	pkGeom->SetPixelShader(NULL);

If you want to have a Geometry object use a different shader:

	VertexShader* pkOtherShader;
	...
	pkGeom->SetVertexShader( pkOtherShader );
	// Set constants
	...

Whenever you set or change the vertex shader attached to a piece of geometry, you must re-set all constants.  Constants are unique to the piece of geometry and not to the shader itself.  Therefore, if you attach a shader to one piece of geometry, set some constants, and then attach it to another piece of geometry you must set those constants for the second piece of geometry.

Uniform constants are accessed by their Cg variable names.  For example, if your Cg program has the following parameters in a vertex shader:

	float4x4 TempMatrix
	float4 MyVariable
	float BlendFactor
	float3 TangentVector
	float4 LookupTable[16]

They are accessed by name within the program by those names:

	ShaderConst* pkConst;
	pkConst = pkGeom->GetVertexConst( "TempMatrix" );
	...
	pkConst = pkGeom->GetVertexConst( "MyVariable" );
	...
	pkConst = pkGeom->GetVertexConst( "LookupTable[3]" );
	...
	pkConst = pkGeom->GetVertexConst( "LookupTable[12]" );
	...

You will notice that uniform array parameters must be accessed individually.  These pointers to ShaderConst objects are valid as long as the particular shader is not unset or changed in the Geometry object.   Once you have accessed the constant, if you want to set data of these variables, you can use the overloaded ShaderConst::SetData function:

    void ShaderConst::SetData (const float* afData);
    void ShaderConst::SetData (const Vector2f& rkData);
    void ShaderConst::SetData (const Vector3f& rkData);
    void ShaderConst::SetData (const Vector4f& rkData);
    void ShaderConst::SetData (const Matrix4f& rkData);
    void ShaderConst::SetData (float fX, float fY = 0.0f, float fZ = 0.0f, float fW = 1.0f );

In the case of Matrix4f, it is required that the parameter be of size float4x4.  In the case of the pointer to float data, it only uses an amount of data corresponding to the size of the uniform constant.  In all other cases, it sets constants in sizes of 4 floats.  For all functions with data less than four floats large (Vector2f, Vector3f, 4 floats), it fills in y and z with zero and w with 1, by default.  If you specify a uniform constant as a float, float2, or float3 you won't be able to access the other elements.  You should note that the Cg Converter will not accept any programs that use types other than float, float2, float3, float4, float3x3, float4x4, and sampler*.


Compatibility Issues
--------------------

If you would like to make your Cg programs compatible with both OpenGL and DirectX you need to follow a few restrictions:

1) No access of GL state from within the Cg program.
2) DirectX programs (at least in 8.1, and with ps1.1-1.3) truncate inputs to pixel shaders into the [0..1] range.  Typically you can munge and unmunge the values to allow for a larger range of inputs.  Within the pixel shader itself a much larger range is acceptable.
3) OpenGL sets the w component of the outgoing position vector in the vertex shader to be one.  DirectX does care and will happily project your points all over the place if it is something other than one.


Shader Versions
---------------

Cg has no support for ps.1.4.  This version of DirectX pixel shaders essentially allows two ps.1.3 shaders to run back to back.  This allows for things such as dependent texture reads and more instructions (that many pixel shaders need).  Many cards (GeForce4TI/Radeon8x00) can do ps.1.4 but only newer cards (GeForceFX/Radeon9x00) have full support for ps.2.0.  If you have a shader that you would really like to be compatible with more cards, then your best bet is to somehow compile it into ps.1.4 and then build your own shader file.  See the building your own shader file section for details.


Rolling your own .wvs/.wps file
-------------------------------

If you would like to create your own shader file from scratch (and not using this utility), it is fairly easy to do.  The best documentation for this is within the CgConverter.cpp file itself as well as in the ShaderConstants.* and StateConstant.* files.  The bulk of the code in the CgConverter is for creating the ShaderConstants object. This might be convenient if you have API-specific shaders that you would like to start using with WildMagic.  There is no requirement that a .wvs or .wps file supports DirectX /and/ OpenGL, but it is certainly convenient to do so if you want your shader file to be cross-api compatible.