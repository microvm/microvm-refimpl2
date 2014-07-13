#include <stdio.h>

int isPrime(long a) {
  for (long i = 2; i < a; i++)
    if (a % i == 0)
      return 0;

  return 1;
}

int main() {
  int x = isPrime(1000000000000000001L);
  printf("%d\n", x);
  return 0;
}
