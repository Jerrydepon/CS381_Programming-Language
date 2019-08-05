#include <stdio.h>

void prStack2(l,y,z) {
  printf("%2d [y:%d, z:%d]\n", l, y, z); }

void prStack(l,x,y,z) {
  printf("%2d [x:%d, f{}, y:%d, z:%d]\n", l, x, y, z); }

main()
{ int z;
  int y;                         prStack2(2,y,z);
  y = 5;                         prStack2(3,y,z);
  { int f(int *xp){ int x=*xp;   prStack(4,x,y,z);
        x = x+1;                 prStack(5,x,y,z);
        y = x-4;                 prStack(6,x,y,z);
        x = x+1;                 prStack(7,x,y,z);
                    *xp=x;       prStack(8,x,y,z);
        return x;
    };                           prStack2(9,y,z);
    z = f(&y)+y;                 prStack2(10,y,z);
  };                             prStack2(11,y,z);
}
