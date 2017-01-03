package ifi.math.spline;

/**
 * The <em>sorting</em> class contains to static functions that can be used
 * for sorting vectors containing doubles.
 */

public class Sorting
{
  /**
   * Generates an index set of the values in the input double array.
   * The indexes are generated so that
   * <tt>vec[index[i]] <= vec[index[i+1]]</tt>.
   *
   * @param size number of elements in the arrays.
   *
   * @param vec the double values to be indexed.
   *
   * @param index the generated indexes.
   * This array must be allocated before calling this method.
   *
   */

  public static void makeIndex (int size, double[] vec, int[] index)
  {
    int l,i,j,ir,indxt;
    double q;
    for (i=0; i<size; i++)
      index[i] = i;

    l = size>>1;
    ir = size-1;
    for (;;) {
      if (l>0)
	q = vec[(indxt=index[--l])];
      else {
	q = vec[(indxt=index[ir])];
	index[ir]=index[0];
	if (--ir==0) {
	  index[0]=indxt;
	  return;
	}
      }
      i = l;
      j = ((l+1)<<1)-1;
      while (j<=ir) {
	if (j<ir && vec[index[j]]<vec[index[j+1]])
	  j++;
	if (q<vec[index[j]]) {
	        index[i] = index[j];
	        j += (i=j)+1;
	}
	else
	  j = ir+1;
      }
      index[i] = indxt;
    }
  }

  /**
   * Sorts the entries in a double array according to an index set.
   * The releation between the entries in the input and output double
   * array is: <tt>vec[i]=vec[index[i]]</tt>.
   *
   * @param size number of elements in the arrays.
   *
   * @param vec contains the double values to be sorted.
   *
   * @param index contains the indexes that should be used for the sort.
   */

  public static void sortAccording2index (int size, double[] vec, int[] index)
  {
    int i;
    double[] tmp = new double[size];
    for (i=0; i<size; i++)
      tmp[i] = vec[i];
    for (i=0; i<size; i++)
      vec[i] = tmp[index[i]];
  }
}
