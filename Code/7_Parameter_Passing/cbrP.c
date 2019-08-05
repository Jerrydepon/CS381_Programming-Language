#include <stdio.h>

void prStack2(l,y,z) {
  printf("%2d [y:%d, z:%d]\n", l, y, z); }

void prStack(l,x,yp,y,z) {
  printf("%2d [x:%d, y(%d):%d, z:%d]\n", l, x, yp, y, z); }

main()
{ int z;
  int y;             prStack2(2,y,z);
  y = 5;             prStack2(3,y,z);
  { int f(int *x){   prStack(4,x,&y,y,z);
        *x = *x+1;   prStack(5,x,&y,y,z);
        y  = *x-4;   prStack(6,x,&y,y,z);
        *x = *x+1;   prStack(7,x,&y,y,z);
        return *x;
    };               prStack2(9,y,z);
    z = f(&y)+y;     prStack2(10,y,z);
  };                 prStack2(11,y,z);
}
