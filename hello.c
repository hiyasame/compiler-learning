int main() {
  int a = 1;
  {
    a = a + 2;
  }
  a = a + 5;
  return a;
}
