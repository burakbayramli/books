// Clase estática que aglutina gran parte de las funciones matemáticas usadas
// por las clases de este paquete.
package sgp;

class FuncionesMatematicas
{
	static double modulus(double d1,double d2)
	{
		double modu;

		modu = d1 - (Math.floor(d1/d2) * d2);
		if (modu >= 0)
			return modu;
		else
			return modu + d2;
	}

	static double fmod2p(double d)
	{
		return modulus(d,(Math.PI*2.0));
	}

	static double acTan(double sinx,double cosx)
	{
		if (cosx==0)
		{
			if (sinx>0)
				return (Math.PI/2.0);
			else
				return (3.0*Math.PI/2.0);
		}
		else
		{
			if (cosx>0)
				return Math.atan(sinx/cosx);
			else
				return Math.PI + Math.atan(sinx/cosx);

		}
	}

	static void magnitud(double[] v)
	{
		v[3] = Math.sqrt((v[0]*v[0])+(v[1]*v[1])+(v[2]*v[2]));
	}

	static void subVectores(double[] v1, double[] v2, double[] v3)
	{
		for (int i = 0;i<3;i++)
			v3[i] = v1[i] - v2[i];
  		FuncionesMatematicas.magnitud(v3);
	}


	// Multiplica el vector v1 por un escalar k para producir res
	static void multEscalarVector(double[] v1, double k, double[] res)
	{
		for (int i=0;i<3;i++)
			res[i] = k*v1[i];
		res[3] = Math.abs(k)*v1[3];
	}

	// Calcula el angulo comprendido entre los vectores v1 y v2
	static double angulo(double[] v1, double[] v2)
	{
		FuncionesMatematicas.magnitud(v1);
		FuncionesMatematicas.magnitud(v2);
		return Math.acos(dot(v1,v2)/(v1[3]*v2[3]));
	}

	static double dot(double[] v1, double[] v2)
	{
		return (v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2]);
	}

	static double toRadians(double arg)
	{
		return (arg*Math.PI/180.0);
	}

	static double toDegrees(double arg)
	{
		return (arg*180.0/Math.PI);
	}

}