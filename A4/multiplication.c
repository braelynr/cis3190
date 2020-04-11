// Braelyn Rotman
// CIS*3190
// Assignment 4
// Due April 9 2020

#include <stdio.h>
#include <sys/time.h>

// HIS IS THE NON-RECURSIVE FUNCTION
long long multiply(int m, int n)
{
  int x, y;
  long long p = 0;
  x = m;
  y = n;
//  x is the Multiplier
//  y is the Multiplicand
//  p is the Product

  while(x > 0)
  {
    if (x == 1)
    {
      return p + y;
    }
    else if (x > 1 && (x % 2 == 0))
    {
      x = x / 2;
      y = y + y;
    }
    else
    {
      p = p + y;
      x = x / 2;
      y = y * 2;
    }
  }
  return 0;
}

// THIS IS THE RECURSIVE FUNCTION
long long multiplyRec(int m, int n)
{
  // m is the Multiplier
  // n is the Multiplicand

  if (m == 0)
  {
    return 0;
  }
  else if (m == 1)
  {
    return n;
  }
  else if (m > 1 && (m % 2) == 0)
  {
    return multiplyRec(m / 2, n + n);
  }
  else
  {
    return n + multiplyRec(m / 2, n * 2);
  }
}

int main()
{
  int m, n;
  long long p;
  struct timeval start, end;
  float execTime, sec, msec;

  printf("Enter Positive Integer 1: ");
  scanf("%d", &m);
  printf("Enter Positive Integer 2: ");
  scanf("%d", &n);

  printf("\nRUSSIAN PEASANT MULTIPLICATION\n");
  printf("Recursive Function:\n");
  // Call recursive function
  gettimeofday(&start, NULL);
  p = multiplyRec(m, n);
  gettimeofday(&end, NULL);
  sec = (end.tv_sec - start.tv_sec);
  msec = (end.tv_usec - start.tv_usec);
  execTime = (1000 * sec) + (msec * 0.001);

  printf("%d x %d = %lli\n", m, n, p);
  printf("Runtime = %f milliseconds\n", execTime);

  printf("\nNon-Recursive Function:\n");
  // call non-recursive function
  gettimeofday(&start, NULL);
  p = multiply(m, n);
  gettimeofday(&end, NULL);
  sec = (end.tv_sec - start.tv_sec);
  msec = (end.tv_usec - start.tv_usec);
  execTime = (1000 * sec) + (msec * 0.001);

  printf("%d x %d = %lli\n", m, n, p);
  printf("Runtime = %f milliseconds\n", execTime);

  return 0;
}
