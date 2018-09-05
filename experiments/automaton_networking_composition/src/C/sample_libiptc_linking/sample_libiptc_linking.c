#include <stdio.h>
#include <stdlib.h>

extern const char *iptc_strerror(int err);

int main()
{
  printf("hello, world\n");
  printf("%s\n", iptc_strerror(0));
  return 0;
}
