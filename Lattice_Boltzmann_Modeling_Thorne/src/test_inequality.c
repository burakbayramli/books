#include <stdio.h>

main()
{
  char str[1024];

  sprintf( str, "ab");

  printf("strlen(\"%s\") = %d\n", str, strlen(str));
  if( 1 - strlen(str) > 0)
  {
    printf("1 - strlen(\"%s\") > 0 = TRUE\n", str);
  }
  else
  {
    printf("1 - strlen(\"%s\") > 0 = FALSE\n", str);
  }

  if( 1 > strlen(str))
  {
    printf("1 > strlen(\"%s\") = TRUE\n", str);
  }
  else
  {
    printf("1 > strlen(\"%s\") = FALSE\n", str);
  }
}
