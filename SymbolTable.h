#define MAX_TABLE_SIZE 5000

typedef struct symbol_entry *PTR_SYMB;
struct symbol_entry {
   char *name;
   int scope;
   int offset;
   int id;
   int variant;
   int type;
   int total_args;
   int total_locals;
   int mode;
}  table[MAX_TABLE_SIZE];

#define T_FUNCTION 1
#define ARGUMENT_MODE   2
#define LOCAL_MODE      4
#define GLOBAL_MODE     8

extern int cur_scope;
extern int cur_counter;
