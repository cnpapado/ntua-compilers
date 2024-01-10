#include <stdio.h>

char readChar() {
  char res;
  scanf("%c", &res);
  return res;
}

void writeChar(char c) {
  printf("%c", c);
}

long long readInteger() {
  long long res;
  scanf("%lld", &res);
  return res;
}

void writeInteger(long long n) {
  printf("%lld", n);
}

void readString(long n, char* s) {
  scanf("%s", s);
  s[n-1] = '\0';
}

void writeString(const char* s) {
  printf("%s", s);
}

int ascii(char c) {
  return (int)c;
}

char chr(int n) {
  return (char)n;
}