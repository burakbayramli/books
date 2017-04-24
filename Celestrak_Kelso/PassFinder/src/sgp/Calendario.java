// En versiones de Java menores que 1.4.1 tienen el problema de que
// el método "setTimeInMillis()" de la clase java.util.Calendar tiene
// visibilidad protected, con lo que no puede ser aplicada a una instancia
// de Calendar. En la versión 1.4.1 se ha cambiado su visibilidad a pública.
// Para que el código sea compilable por todas las versiones, he decidido
// crear una clase MiCalendario que hereda de Calendar y convierte la
// visibilidad del método a public.
package sgp;

import java.util.Calendar;

abstract class Calendario extends Calendar
{
	public void	setTimeInMillis(long millis)
	{
		this.setTimeInMillis(millis);
	}
}