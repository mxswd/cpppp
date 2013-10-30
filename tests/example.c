int main(void) {
  int x = 2;
  int y = 4;
  int z = @|objc_hs|"(+) %d %d"|x, y|;
  printf("%d", z);
  return 0;
}
