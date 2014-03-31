// TYPES
#define NUMBER 0
#define BOOLEAN 1
#define ERROR 2
#define STRING 3
#define NAME 4
#define PRIMITIVE 5

// ERROR
#define ERRORKIND 0

// BOOLEAN
#define TRUE 1
#define FALSE 0

// PRIMITIVES
//  ARITHMETIC
#define ADD 0
#define SUB 1
#define MUL 2
#define DIV 3
#define REM 4

//  OTHERS
#define BIND 5
#define LOAD 6
#define POP  7
#define EXC  8
#define QUIT 9

struct Type {
  int type;
  union {
    int number;
    int bool;
    int error;
    char* string;
    char* name;
    int primitive;
  } value;
};
