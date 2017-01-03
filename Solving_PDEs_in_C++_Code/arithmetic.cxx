//  ARITHMETIC EXPRESSION (cHAPTER 1.21)
//  ---------------------------------
//
//This program takes an integer arithmetic expression in infix form
//and returns its prefix and postfix forms (with no parentheses)
//and its integer value.
//  The complete documentation is in the book.
//

#include<stdio.h>

void copy(char* t, char* s, int n){
  for(int i=0;i<n;i++)
    t[i]=s[i];
  t[n]='\n';
}

int fix(char* s, int length, int post){
  for(int i=length-1;i>=0;i--)
    if((s[i]=='+')||(s[i]=='-')){
      char s1[i+1];
      char s2[length-i];
      copy(s1,s,i);
      copy(s2,s+i+1,length-i-1);
      if(post==2){
	if(s[i]=='+')
	  return fix(s1,i,post) + fix(s2,length-i-1,post);
	else
	  return fix(s1,i,post) - fix(s2,length-i-1,post);
      }
      if(post==0)printf("%c",s[i]);
      fix(s1,i,post);
      fix(s2,length-i-1,post);
      if(post==1)printf("%c",s[i]);
      return 0;
    }
  for(int i=length-1;i>=0;i--)
    if(s[i]=='%'){
      char s1[i+1];
      char s2[length-i];
      copy(s1,s,i);
      copy(s2,s+i+1,length-i-1);
      if(post==2)return fix(s1,i,post) % fix(s2,length-i-1,post);
      if(post==0)printf("%c",s[i]);
      fix(s1,i,post);
      fix(s2,length-i-1,post);
      if(post==1)printf("%c",s[i]);
      return 0;
    }
  for(int i=length-1;i>=0;i--)
    if((s[i]=='*')||(s[i]=='/')){
      char s1[i+1];
      char s2[length-i];
      copy(s1,s,i);
      copy(s2,s+i+1,length-i-1);
      if(post==2){
	if(s[i]=='*')
	  return fix(s1,i,post) * fix(s2,length-i-1,post);
	else
	  return fix(s1,i,post) / fix(s2,length-i-1,post);
      }
      if(post==0)printf("%c",s[i]);
      fix(s1,i,post);
      fix(s2,length-i-1,post);
      if(post==1)printf("%c",s[i]);
      return 0;
    }
  if(*s == '\n'){
    printf("error");
    return 0;
  }
  if(post==2){
    int sum=0;
    int exp=1;
    for(int i=length-1;i>=0;i--){
      if((s[i]>='0')&&(s[i]<='9')){
        sum += (s[i]-'0') * exp;
	exp *= 10;
      }
      else{
        printf("error");
        return 0;
      }
    }
    return sum;
  }
  for(int i=0;i<length;i++){
    if((s[i]>='0')&&(s[i]<='9'))
      printf("%c",s[i]);
    else{
      printf("error");
      return 0;
    }
  }
  return 0;
}

main(){
  char* s = "3*5-5*3*1";
  int i = 9;
  //for(i=0; (s[i]=getchar()) != '\n'; i++);
  fix(s,i,0);    //prefix form
  printf("\n");
  fix(s,i,1);    //postfix form
  printf("\n");
  printf("%d\n",fix(s,i,2));    //integer value
  return 0;
}
