%{
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int* concat_list(int*, int*);
int pow_func(int, int);
void print_list(int*);
int* append_list(int*, int);
int yyerror (char *s);
int yylex();
void init_table();
void put_token(char id[20], int value);
int get_token(char id[20]);
#define total_token 20

%}

%union{
    int num;
    int *nums;
    char id[20];
}

%start INPUT
%token STRING 
%token COMMENT 
%token OP_MULT 
%token OP_OP 
%token OP_CP 
%token OP_DBLMULT 
%token OP_CC 
%token OP_COMMA
%token OP_PLUS 
%token OP_MINUS 
%token OP_DIV 
%token OP_OC 
%token KW_SET 
%token KW_DEFFUN 
%token KW_FOR 
%token KW_IF 
%token KW_EXIT
%token KW_LOAD 
%token KW_DISP 
%token KW_TRUE 
%token KW_FALSE  
%token KW_AND 
%token KW_OR 
%token KW_NOT 
%token KW_EQUAL
%token KW_LESS
%token KW_NIL 
%token KW_LIST 
%token KW_APPEND 
%token KW_CONCAT 
%token NEWLINE


%token <num> VALUE
%token <id> ID


%type <num> INPUT
%type <num> EXPI
%type <num> EXPB
%type <nums> VALUES
%type <nums> EXPLISTI
%type <nums> LISTVALUE

%%

INPUT: 
    EXPI {printf("SYNTAX OKEY. \nResult = %d\n", $1);}
    |
    EXPLISTI {printf("SYNTAX OKEY. \nResult = "); print_list($1);}
    ;

EXPI:
    OP_OP OP_PLUS EXPI EXPI OP_CP  {$$=$3+$4;} 
    |
    OP_OP OP_MINUS EXPI EXPI OP_CP {$$=$3-$4;} 
    |
    OP_OP OP_DIV EXPI EXPI OP_CP   {$$=$3/$4;} 
    |
    OP_OP OP_MULT EXPI EXPI OP_CP  {$$=$3*$4;}
    |
    OP_OP KW_LESS EXPI EXPI OP_CP { $$ = $3 < $4; }
    |
    OP_OP KW_EQUAL EXPI EXPI OP_CP {$$ = ($3 == $4);} 
    |
    ID {$$ = get_token($1);}
    |
    VALUE {$$ = $1;}
    |
    OP_OP KW_SET ID EXPI OP_CP {$$ = $4; put_token($3, $4);}
    |
    OP_OP KW_IF EXPB EXPI OP_CP {$$ = (1 == $3) ? $4: 0;}
    |
    OP_OP KW_AND EXPB EXPB OP_CP {$$ = $3 && $4;}   
    |
    OP_OP KW_OR EXPB EXPB OP_CP  {$$ = $3 || $4;}
    |
    OP_OP KW_NOT EXPB OP_CP  {$$ = ! ($3);}   
    |
    OP_OP KW_EQUAL EXPB EXPB OP_CP {$$ = ($3 == $4);}  
    |
    OP_OP OP_DBLMULT EXPI EXPI OP_CP {$$ = pow_func($3, $4);}
    |
    OP_OP KW_DISP EXPI OP_CP { $$ = $3; printf("Print: %d\n", $3);} 
    ;

EXPB:
    OP_OP KW_FOR EXPB EXPI OP_CP { $$ = (1 == $3) ? $4 : 0; }
    |
    KW_TRUE  { $$ = 1; }   /* true */
    |
    KW_FALSE   { $$ = 0; } /* false */
    | 
    OP_OP KW_DISP EXPB OP_CP { $$ = $3; printf("Print: %s\n", ($3 ? "true":"false"));}
    ;

EXPLISTI:
    OP_OP KW_APPEND EXPI EXPLISTI OP_CP {$$ = append_list($4, $3);}
    |
    OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP {$$ = concat_list($3, $4);}
    |
    OP_OP KW_LIST VALUES OP_CP {$$ = $3;}
    |
    LISTVALUE  {$$ = $1;}
    |
    OP_OP KW_DISP LISTVALUE OP_CP { $$ = $3; printf("Print: "); print_list($3);}
    ;

LISTVALUE:  
    OP_OP VALUES OP_CP {$$ = $2;}
    |
    OP_OP OP_CP { $$= NULL; }
    |
    KW_NIL { $$ = NULL;}
    ;

VALUES: 
    VALUES VALUE  {$$ = append_list($1, $2);}
    |
    VALUE {$$ = NULL; $$ = append_list($$, $1);}
    ;
%%
struct token {
	char id[20]; 
	int value;
};

struct symbol_table {
	struct token table[total_token];
	int valid_token;
};

struct symbol_table *symtab;

int yyerror(char *s) {
    fprintf(stderr, "SYNTAX ERROR. \n");
    return 0;
}

void print_list(int *list){

    printf("( ");

    for(int i=0;list[i]!=-1; ++i){
        printf("%d ", list[i]);
    }

    printf(")\n");

}

void init_table(){
	symtab = (struct symbol_table*) malloc(sizeof(struct symbol_table));
	symtab->valid_token = 0;
}

void put_token(char id[20], int value){
	struct token *e;
	e = (struct token*) malloc(sizeof(struct token));
	e->value = value;
	strcpy(e->id, id);

	int i=0;
	for(i;i<symtab->valid_token; ++i){
		if(strcmp(e->id, symtab->table[i].id) == 0){
			symtab->table[i].value = e->value; 
			return;
		}
	}

	strcpy(symtab->table[i].id, e->id); 
	symtab->table[i].value = e->value; 
	++(symtab->valid_token);
	free(e);
}

int get_token(char id[20]){

	int i=0;


	for(i;i<symtab->valid_token; ++i){
		if(strcmp(id, symtab->table[i].id) == 0) 
			return symtab->table[i].value;
	}

	printf("Symbol not found!\n Exiting...");
	exit(-1);
}
int* append_list(int *list, int num){ 
    if(list == NULL){
        list = (int *)malloc(sizeof(int)*2);
        list[0] = num;
        list[1] = -1;
    } 
    else{ 
        int *temp = list;
        int size = 0;

        while(*temp != -1){
            ++temp;
            ++size;
        }

        temp = list;
        list = (int*)(malloc(sizeof(int)*(size+2)));

        int i=0;
        for(i;i<size;++i)
            list[i] = temp[i];
        list[i] = num; 
        list[i+1] = -1;
        free(temp);
    } 

    return list;     
}

int* concat_list(int *list1, int *list2){
    int size1=0, size2=0;
    int i=0,j=0;
    int *temp = list1;
    
    while(*temp != -1){
        size1++;
        temp++;
    }

    temp = list2;
    while(*temp != -1){
        size2++;
        temp++;
    }

    temp = (int *) malloc(sizeof(int) * (size1 + size2) + 2);

    
    for(i;i<size1;++i) 
        temp[i] = list1[i];
    
   
    for(j;j<size2; ++j) 
        temp[i++] = list2[j]; 

    temp[i] = -1;
    free(list1);
    free(list2);
    return temp;
}

int pow_func(int b, int pow) {
    if (pow != 0)
        return (b * pow_func(b, pow - 1));
    else
        return 1;
}

int main(int argc, char **argv)
{

    ++argv, --argc;
    init_table();

    printf("Enter String: ");
    while(1){
        yyparse();
    }

    return 0;
}

