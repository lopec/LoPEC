/**
 * NAME        : port.c
 * DESCRIPTION : A example C program to communicate with Erlang
 *               using pipes. Represents a C host.
 * VERSION     : 0.0.2
 * OWNER       : Bjorn Dahlman
 */

#include <stdio.h>
#include <string.h>

void map(const char *loadPath, const char *savePath);
void reduce(const char *loadPath, const char *savePath);

int main(int argc, char *argv[])
{
  if (!strcmp(argv[1], "map"))
    map(argv[2], argv[3]);
  else if (!strcmp(argv[1], "reduce"))
    reduce(argv[2], argv[3]);
  else {
    fprintf(stdout,"olol ditt jaevla fitthor, ge mig riktiga varden");
    return(1);
  }

  return 0;
}

void map(const char *loadPath, const char *savePath)
{
  fprintf(stdout,"oh no you didn't!");
}

void reduce(const char *loadPath, const char *savePath)
{
  fprintf(stdout,"oh yes he did!");
}
