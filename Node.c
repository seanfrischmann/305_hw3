#include <stdlib.h>

struct Node {
  struct Type * car;
  struct Node * cdr;
};

// LIST HAD A HEADER NODE, THEN LIST ELEMENTS
struct Node * cons(struct Type * car, struct Node * cdr) {
  struct Node * header;
  header = (struct Node *) (malloc(sizeof(struct Node)));
  (*header).car = car;
  (*header).cdr = cdr;
  return header;
}

struct Node * makeEmptyList() {
  return cons(NULL,NULL);
}

struct Node * push(struct Node * list, struct Type * datum) {
  struct Node * newNode = cons(datum,NULL);
  (*newNode).cdr = (*list).cdr;
  (*list).cdr = newNode;
  return list;
}

int isEmpty(struct Node * list) {
  return (*list).cdr == NULL;
}

struct Type * pop(struct Node * list) {
  if (isEmpty(list)) { return NULL; }
  struct Type * top = (*(*list).cdr).car;
  (*list).cdr = (*(*list).cdr).cdr;
  return top;
}
