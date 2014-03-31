/* CSE305 Spring 2014
 * A possible solution to HW2, expressed in C
 * Author: Carl Alphonce & Daniel Bellinger 
 * 
 * New Features Added by: Sean Frischmann
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "Node.c"
#include "Type.c" 

#define BUFFER_LENGTH 256


/*
 * Data for bindings
 * New Feature
 */
typedef struct Data{
	 char *key;
	 struct Type * value;
	 struct Data *next;
}data_points;

/*
 * Map for bindings
 * New Feature
 */
typedef struct Map{
	 struct Data * start;
}map;

/*
 * Add bindings to Map
 * New Feature
 */
void addToMap(map *list, char *key, struct Type *value){
	data_points *temp;
	temp = (struct Data *) (malloc(sizeof(struct Data)));
	temp->key = key;
	temp->value = value;
	temp->next = list->start;
	list->start = temp;
}

/*
 * Find Key
 * New Feature
 */
int findInMap(map *list, char *key){
	 data_points *current = list->start;
	 while(current != NULL){
		if(strcmp(current->key, key)==0) {return TRUE;}
			current = current->next;
	 }
	 return FALSE;
}

struct Type * makeError() {
	 struct Type * x = (struct Type *) (malloc(sizeof(struct Type)));;
	 (*x).type = ERROR;
	 (*x).value.error = ERRORKIND;
	 return x;
}

/*
 * Find Value
 * New Feature
 */
struct Type * findValue(map *list, char *key){
	 data_points * current = list->start;
	 struct Type * temp;
	 while (current != NULL){
		  if(strcmp(current->key, key)==0) { 
				if((current->value->type == NAME)&&(findInMap(list, current->value->value.name))){
					key = current->value->value.name;
					current = list->start;
					continue;
				}else{
					 temp = current->value;
				}return temp;
		  }
		  current = current->next;
	 }
	 return makeError();
}

struct Type * makeNumber(int num) {
    struct Type * px = (struct Type *) (malloc(sizeof(struct Type)));;
    (*px).type = NUMBER;
    (*px).value.number = num;
    return px;
}

struct Type * makeBoolean(int b) {
    struct Type * px = (struct Type *) (malloc(sizeof(struct Type)));;
    (*px).type = BOOLEAN;
    (*px).value.bool = b;
    return px;
}
/*
 * Creates Name
 *   if name is binded uses value instead
 * New Feature
 */
struct Type * makeName(char *b, map *bindings) {
	 if(findInMap(bindings, b) == TRUE){
		 return findValue(bindings, b);
	 }
	 struct Type * px;
	 px = (struct Type *) (malloc(sizeof(struct Type)));;
	 char * temp = (char *) (malloc(strlen(b)+1));
	 strncpy(temp,b, strlen(b)+1);
	 (*px).type = NAME;
	 (*px).value.string = temp;
	 return px;
}

/*
 * Creates string
 * New Feature
 */
struct Type * makeString(char * b) {
	 struct Type * px = (struct Type *) (malloc(sizeof(struct Type)));;
	 char * temp = (char *) (malloc(strlen(b)));
	 int i;
	 int j = 0;
	 int length = strlen(b)-1;
	 for(i=1;i<length;i++){
		  temp[j] = b[i];
		  j = j+1;
	 }
	 (*px).type = STRING;
	 (*px).value.string = temp;
	 return px;
}

int isBoolean(struct Type * t) { return (*t).type == BOOLEAN; }
int isError(struct Type * t)   { return (*t).type == ERROR; }
int isNumber(struct Type * t)  { return (*t).type == NUMBER; }
int valueOf(struct Type * t)   { return (*t).value.number; }
/*
 * Checks if type is name
 * New Feature
 */
int isName(struct Type * t) { return (*t).type == NAME; }
/*
 * Checks if type is string
 * New Feature
 */
int isString(struct Type * t) { return (*t).type == STRING; }

/*
 * Checks if token is a letter
 * New Feature
 */
int isLetter(char * token) { 
	 if(((token[0]-0)>64)&&((token[0]-0)<91)){return TRUE;}
	 else if(((token[0]-0)>96)&&((token[0]-0)<123)) {return TRUE;}
	 else{return FALSE;}
}

/*   Pop top value from stack	*/
void popStack(struct Node *stack){
  struct Type * t;
  if((t = pop(stack)) == NULL){
    push(stack,makeError());
  }
}

/*   Swap top 2 values of stack	  */   

void excStack(struct Node *stack){
  struct Type * x = pop(stack);
  struct Type * y = pop(stack);
  //make sure there are 2 values to exchange
  if(x == NULL){ //no values on the stack
    push(stack,makeError());
  }else if(y == NULL){ //only 1 value was on the stack
  	push(stack,x);
  	push(stack,makeError());
  }else{
  	push(stack, x);
  	push(stack, y);
  }
}

/*   negate value   */

struct Type * negate(struct Type * number){
  if (isNumber(number)) { return makeNumber(-valueOf(number)); }
  return makeError();
}

/* Bind function
 * New Feature
 */ 

struct Type * bind(struct Type * x, struct Type * y){
  if (isName(x)) {

  }
  return makeError();
}

/* And function
 * New Feature
 */ 

struct Type * and(struct Type * x, struct Type * y){
  if (isBoolean(x) && isBoolean(y)) {
		if ((*x).value.bool == (*y).value.bool) { return makeBoolean(TRUE);}
		else {return makeBoolean(FALSE);}
  }
  return makeError();
}

/* or function
 * New Feature
 */ 

struct Type * or(struct Type * x, struct Type * y){
  if (isBoolean(x) && isBoolean(y)) {
		if ((*x).value.bool != (*y).value.bool) { return makeBoolean(TRUE);}
		else {return makeBoolean(FALSE);}
  }
  return makeError();
}

/* not function
 * New Feature
 */ 

struct Type * not(struct Type * x){
  if (isBoolean(x)) {
		if ((*x).value.bool == 0) { return makeBoolean(TRUE);}
		else {return makeBoolean(FALSE);}
  }
  return makeError();
}

/* equal function
 * New Feature
 */ 

struct Type * equal(struct Type * x, struct Type * y){
  if (isNumber(x) && isNumber(y)) {
		if ((*x).value.bool == (*y).value.bool) { return makeBoolean(TRUE);}
		else {return makeBoolean(FALSE);}
  }
  return makeError();
}

/* lessThan function
 * New Feature
 */ 

struct Type * lessThan(struct Type * x, struct Type * y){
  if (isNumber(x) && isNumber(y)) {
		if ((*x).value.bool > (*y).value.bool) { return makeBoolean(TRUE);}
		else {return makeBoolean(FALSE);}
  }
  return makeError();
}

/*   Add function   */

struct Type * add(struct Type * num1, struct Type * num2){
  if (isNumber(num1) && isNumber(num2)) {
    return makeNumber(valueOf(num1) + valueOf(num2));
  }
  return makeError();
}

/*   Subtract function	 */

struct Type * sub(struct Type * num1, struct Type * num2){
  if (isNumber(num1) && isNumber(num2)) {
    return makeNumber(valueOf(num1) - valueOf(num2));
  }
  return makeError();
}


/*  Multiply function	*/

struct Type * mul(struct Type * num1, struct Type * num2){
  if (isNumber(num1) && isNumber(num2)) {
    return makeNumber(valueOf(num1) * valueOf(num2));
  }
  return makeError();
}

/*   Divide function   */

int c_divide(int a, int b) {
  if (b<0) { return b/a + (a<0? 1 : -1); }
  else     { return b/a; }
}

struct Type * divide(struct Type * num1, struct Type * num2){
  if (isNumber(num1) && isNumber(num2)) {
    return makeNumber( c_divide(valueOf(num1), valueOf(num2)) );
  }
  return makeError();
}

/*   Remainder function	  */

struct Type * rem(struct Type * num1, struct Type * num2){
  if (isNumber(num1) && isNumber(num2)) {
    return makeNumber( valueOf(num2) - valueOf(num1) * valueOf(divide(num1,num2)) );
  }
  return makeError();
}


/* toString() for each type   */

char * expression2string(struct Type * t){
  char * str = (char *) malloc(sizeof(char)*255);
  switch( (*t).type ){
  case NUMBER:
    sprintf(str,"%d",(*t).value.number);
    break;
  case BOOLEAN:
    str = (*t).value.bool == TRUE ? ":true:" : ":false:";
    break;
  case ERROR:
    str = ":error:";
    break;
  case STRING:
    sprintf(str,"\"%s\"",(*t).value.string);
    break;
  case NAME:
    sprintf(str,"%s",(*t).value.string);
    break;
  }   
  return str;
}


/*   Print out the stack   */

void printStack(struct Node *stack){
  // loop through stack list, toString each value, print
  stack = stack->cdr; // skip past header node
  while(stack != NULL){
    printf("%s\n",expression2string(stack->car));
    stack = stack->cdr;
  }
}

/*   Apply different types of operators   */

void applyBinary(struct Type * (*f)(), struct Node * stack) {
  struct Type * x = pop(stack);
  struct Type * y = pop(stack);
  if (y != NULL && x != NULL) {
    struct Type * result = (*f)(x,y);
    if ((*result).type == ERROR) {
      push(stack,y);
      push(stack,x);
    }
    push(stack,result);
  }
  else if (y == NULL && x == NULL) {
    // stack was empty
    push(stack,makeError());
  }
  else { 
    // stack had ONE item on it
    push(stack,x);
    push(stack,makeError());
  }
}

void applyIf(struct Node * stack) {
	struct Type * last = pop(stack);
	struct Type * middle = pop(stack);
	struct Type * first = pop(stack);
	if (last != NULL && middle != NULL && first != NULL) {
		if (last->type != BOOLEAN) {
			push(stack,first);
			push(stack,middle);
			push(stack,makeError());
		}else if (last->value.bool == TRUE){
			push(stack,first);
		}else{
			push(stack,middle);
		}
	}
	else if (last == NULL && middle == NULL && first == NULL) {
		// stack was empty
		push(stack,makeError());
	}
	else if (middle == NULL && first == NULL){ 
		// stack had ONE item on it
		push(stack,last);
		push(stack,makeError());
	}else{
		push(stack,middle);
		push(stack,last);
		push(stack,makeError());
	}

}

/*
 * Binds to values from the stack
 * New Feature
 */
void applyBindings(struct Node * stack, map *bindings) {
	 struct Type * x = pop(stack);
	 struct Type * y = pop(stack);
	 if (y != NULL && x != NULL) {
		struct Type * result;
		if ((*y).type == NAME){
			if(findInMap(bindings, y->value.name) == TRUE){
					 result = makeError();
			}else{
				addToMap(bindings, y->value.name, x);
				result = x;
			}
		  }else{
			result = makeError();
	 }
	 if ((*result).type == ERROR) {
		push(stack,y);
		push(stack,x);
	 }
	 push(stack,result);
	 }
	 else if (y == NULL && x == NULL) {
	 // stack was empty
	 push(stack,makeError());
	 }
	 else { 
	 // stack had ONE item on it
	 push(stack,x);
	 push(stack,makeError());
	 }
}

void applyUnary(struct Type * (*f)(), struct Node * stack) {
  struct Type * x = pop(stack);
  if (x != NULL) {
    struct Type * result = (*f)(x);
    if ((*result).type == ERROR) {
      push(stack,x);
    }
    push(stack,result);
  }
  else { // stack was empty
    push(stack,makeError());
  }
}

/*
 * Updated to incorporate new features
 */
struct Node * eval(char * token,struct Node *stack, map *bindings){
	 if(strcmp(token,":true:")  == 0) { push(stack,makeBoolean(TRUE)); }
	 else if(token[0] == '"') { push(stack,makeString(token)); }
	 else if(strcmp(token,":false:") == 0) { push(stack,makeBoolean(FALSE)); }
	 else if(strcmp(token,":error:") == 0) { push(stack,makeError()); }
	 else if(strcmp(token,"equal")== 0)      { applyBinary(equal, stack); }
	 else if(strcmp(token,"lessThan")== 0)      { applyBinary(lessThan, stack); }
	 else if(strcmp(token,"and")== 0)      { applyBinary(and, stack); }
	 else if(strcmp(token,"or")== 0)      { applyBinary(or, stack); }
	 else if(strcmp(token,"not")== 0)      { applyUnary(not, stack); }
	 else if(strcmp(token,"add")== 0)      { applyBinary(add, stack); }
	 else if(strcmp(token,"sub")== 0)      { applyBinary(sub, stack); }
	 else if(strcmp(token,"mul")== 0)      { applyBinary(mul, stack); }
	 else if(strcmp(token,"div")== 0)      { applyBinary(divide, stack); }
	 else if(strcmp(token,"rem")== 0)      { applyBinary(rem, stack); }
	 else if(strcmp(token,"neg")== 0)      { applyUnary(negate, stack); }
	 else if(strcmp(token,"pop")== 0)      { popStack(stack); }
	 else if(strcmp(token,"exc")== 0)      { excStack(stack); }
	 else if(strcmp(token,"bind")== 0)      { applyBindings(stack, bindings); }
	 else if(strcmp(token,"if")== 0)      { applyIf(stack); }
	 else if(strcmp(token,"load")== 0)      { 
		 while (1){
			struct Type * x = pop(stack);
			char y[80];
			int i = 0;
			if(x->type != STRING){
				push(stack,x);
				push(stack,makeError());
				break;
			}
			 FILE *opened_file;
			 opened_file =fopen(x->value.string,"r");
			 if(opened_file == NULL){
				push(stack,x);
				push(stack,makeError());
				break;
			 }
			 int c;
			 while((c = fgetc(opened_file)) != EOF){
				 if(c == ' ' || c == '\n'){
					char z[i+1];
					strncpy(z,y,i+1);
					eval(z,stack,bindings);
					memset(&y,0,sizeof(y));
					i=0;
				 }else{
					y[i] = c;
					i = i + 1;
				 }
			 }
			 push(stack,makeBoolean(TRUE));
			 break;
		 }
	 }
	 else if(strcmp(token,"quit")== 0)     { exit(0); }
	 else if(strspn(token, "0123456789-") == strlen(token)) { push(stack,makeNumber(atoi(token))); }
	 else if(isLetter(token) == TRUE) { push(stack,makeName(token, bindings)); }
	 else { push(stack,makeError()); }
	 return stack;
}

/*
 * Main repl function
 * updated to incorporate new features
 */

void repl(){

	 char * buffer;
	 char * token;
	 char storeBuffer[BUFFER_LENGTH];
	 size_t buffLength = 0;
	 struct Node *stack;
	 stack = makeEmptyList(); // make a stack to hold tokens
	 map *bindings;  // make map to hold bindings
	 bindings = (map *) (malloc(sizeof(map)));
	 bindings->start = NULL;


	 while(1){
	 printf("repl>");
	 getline(&buffer,&buffLength,stdin);
	 strcpy(storeBuffer,buffer);
	 storeBuffer[strlen(storeBuffer)-1] = '\0';
		
	 /* get the first token */
	 token = strtok(storeBuffer, " \n\t");

	 /* walk through other tokens */
	 while( token != NULL ) {
		stack = eval(token,stack, bindings);
		token = strtok(NULL, " \n\t");
	 }		 
	 printStack(stack);
	 }
}

/*   Main Method   */

int main(){
  repl();
  return 0;
}
