#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "src/hash.h"
#define STACK_SIZE 1000
#define BUFFER_SIZE 1000
#define TABLE_SIZE 1000
void do_char(char *);
void eval(char *);
typedef enum {false,true} bool;
long vars[26]; // a-z
long stack[STACK_SIZE];
long stack_index=0;
table_t *env;
void push(long x)
{
	if (stack_index==STACK_SIZE) {
		puts("Fatal: Stack overflow");
		exit(1);
	}
	stack[stack_index++]=x;
}
long pop()
{
	if (!stack_index) {
		puts("Fatal: Stack underflow");
		exit(1);
	}
	return stack[--stack_index];
}
void stack_move(char *start,char *end)
{ // Unused
	long items[255];
	for (char *c=start;*c;c++)
		items[(int)*c]=pop();
	for (char *c=end;*c;c++)
		push(items[(int)*c]);
}
char *copy_block(char *start,char *end)
{
	int len=end-start;
	return memcpy(calloc(len+1,1),start,len);
}
void do_char(char *pointer)
{
	static bool string_mode=false;
	static bool int_mode=false;
	static bool char_mode=false;
	static bool comment=false;
	static char *open_bracket=NULL;
	static int bracket_counter=0;
	static char *open_paren=NULL;
	char instr=*pointer;
	// Comments, string printing
	if (string_mode) {
		if (instr=='"')
			string_mode=false;
		else
			putchar(instr);
		return;
	}
	if (instr=='}') {
		comment=false;
		return;
	} else if (instr=='{') {
		comment=true;
		return;
	} else if (comment)
		return;
	// Character literals
	if (char_mode) {
		push((long)instr);
		char_mode=false;
		return;
	}
	// Blocks
	if (instr=='[') {
		bracket_counter++;
		if (open_bracket)
			return;
		open_bracket=pointer;
		return;
	} else if (instr==']') {
		if (!open_bracket) {
			puts("Fatal: Unmatched ]");
			exit(1);
		}
		bracket_counter--;
		if (!bracket_counter) {
			push((long)copy_block(open_bracket+1,pointer));
			open_bracket=NULL;
		}
		return;
	} else if (open_bracket)
		return;
	// Hash table keys
	if (instr=='(') {
		open_paren=pointer;
		return;
	} else if (instr==')') {
		if (!open_paren) {
			puts("Fatal: Unmatched )");
			exit(1);
		}
		char *var=copy_block(open_paren+1,pointer);
		bucket_t *b=get_bucket(env,var);
		if (!b)
			b=add_entry(env,var,NULL);
		free(var);
		push((long)&b->val);
		open_paren=NULL;
		return;
	} else if (open_paren)
		return;
	// Numbers
	if ('0'<=instr&&instr<='9') {
		if (int_mode) {
			push(instr-'0'+pop()*10);
		} else {
			push(instr-'0');
			int_mode=true;
		}
		return;
	} else
		int_mode=false;
	// Variables
	if ('a'<=instr&&instr<='z') {
		push((long)&vars[instr-'a']);
		return;
	}
	// Commands
	long a,b,c;
	switch (instr) {
	case ' ':
		break;
	case '\'':
		char_mode=true;
		break;
	// Stack manipulation
	case '$':
		push(stack[stack_index-1]);
		break;
	case '%':
		if (stack_index)
			pop();
		break;
	case '\\':
		a=pop();
		b=pop();
		push(a);
		push(b);
		break;
	case '@':
		a=pop();
		b=pop();
		c=pop();
		push(b);
		push(a);
		push(c);
		break;
	case 'O':
		push(stack[stack_index-2-pop()]);
		break;
	// Arithmetic/logic
	case '+':
		push(pop()+pop());
		break;
	case '-':
		a=pop();
		b=pop();
		push(b-a);
		break;
	case '*':
		push(pop()*pop());
		break;
	case '/':
		a=pop();
		b=pop();
		push(b/a);
		break;
	case '_':
		push(-pop());
		break;
	case '|':
		push(pop()|pop());
		break;
	case '&':
		push(pop()&pop());
		break;
	case '~':
		push(~pop());
		break;
	case '=':
		push(pop()==pop()?~0:0);
		break;
	case '>':
		push(pop()>=pop()?0:~0);
		break;
	// Variable manipulation
	case ';':
		push(*(long *)pop());
		break;
	case ':':
		a=pop(); // Variable
		b=pop(); // Value
		*(long *)a=b;
		break;
	// Input/Output
	case '"':
		string_mode=true;
		break;
	case '.':
		printf("%ld",pop());
		break;
	case ',':
		putchar((char)pop());
		break;
	case '^':
		push((long)fgetc(stdin));
		break;
	// Execution
	case '!':
		eval((char *)pop());
		break;
	case '?':
		a=pop();
		if (pop()==~0)
			eval((char *)a);
		break;
	case '#':
		a=pop(); // Body
		b=pop(); // Condition
		eval((char *)b);
		while (pop()==~0) {
			eval((char *)a);
			eval((char *)b);
		}
		break;
	// New features
	case 'Q': // Quit
		exit(0);
	case 'C': // Clear
		stack_index=0;
		break;
	case 'S': // Stack
		for (int i=0;i<stack_index;i++)
			printf("%ld ",stack[i]);
		break;
	case 'N': // Newline
		putchar('\n');
		break;
	case 'P': // Print
		fputs((char *)pop(),stdout);
		break;
	case 'M': // Malloc
		push((long)malloc(sizeof(long)*pop()));
		break;
	case 'F': // Free
		free((long *)pop());
		break;
	}
}
void eval(char *str)
{
	for (char *s=str;*s;s++)
		do_char(s);
}
int main(int argc,char **argv)
{
	env=new_table(TABLE_SIZE);
	char input[BUFFER_SIZE];
	for (;;) {
		*input='\0';
		fgets(input,BUFFER_SIZE,stdin);
		if (!*input)
			exit(0);
		eval(input);
	}
	return 0;
}
