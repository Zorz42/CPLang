#include<stdio.h>
#include<stdlib.h>

void func0(){
  int var0;
  var0 = 10;
  10 = 12;
  printf("%s","hello ");
  printf("%d",var0);
  printf("%s","\n");
}

int main(){
    func0();
    return 0;
}
    