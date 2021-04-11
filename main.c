#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <errno.h>
#include <stdbool.h>
#include <limits.h>
 
#define NELEMS(arr) (sizeof(arr) / sizeof(arr[0]))
 
#define da_dim(name, type)  type *name = NULL;          \
                            int _qy_ ## name ## _p = 0;  \
                            int _qy_ ## name ## _max = 0
#define da_rewind(name)     _qy_ ## name ## _p = 0
#define da_redim(name)      do {if (_qy_ ## name ## _p >= _qy_ ## name ## _max) \
                                name = realloc(name, (_qy_ ## name ## _max += 32) * sizeof(name[0]));} while (0)
#define da_append(name, x)  do {da_redim(name); name[_qy_ ## name ## _p++] = x;} while (0)
#define da_len(name)        _qy_ ## name ## _p
 
typedef enum {
    tk_EOI, tk_Assign1, tk_GreaterEq, tk_LessEq, tk_Compare, tk_Colon, tk_Assign2, tk_Add, tk_Subtract,
    tk_Multiply, tk_Divide, tk_Modulus, tk_Dot, tk_Less, tk_Greater, tk_Lparent, tk_Rparent, tk_Lbrace, tk_Rbrace, 
    tk_Semicolon, tk_Comma, tk_Lbracket, tk_Rbracket, tk_Begin, tk_End, tk_Loop, tk_While. tk_Void, tk_Exit,
    tk_Getter, tk_Outter, tk_Main, tk_If, tk_Then, tk_Assign, tk_Data, tk_Proc, tk_Ident, tk_Integer, tk_String
} TokenType;
 
typedef struct {
    TokenType tok;
    int err_ln, err_col;
    union {
        int n;                  /* value for constants */
        char *text;             /* text for idents */
    };
} tok_s;
 
static FILE *source_fp, *dest_fp;
static int line = 1, col = 0, the_ch = ' ';
da_dim(text, char);
 
tok_s gettok(void);
 
static void error(int err_line, int err_col, const char *fmt, ... ) {
    char buf[1000];
    va_list ap;
 
    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    printf("(%d,%d) error: %s\n", err_line, err_col, buf);
    exit(1);
}
 
static int next_ch(void) {     /* get next char from input */
    the_ch = getc(source_fp);
    ++col;
    if (the_ch == '\n') {
        ++line;
        col = 0;
    }
    return the_ch;
}
 
static tok_s char_lit(int n, int err_line, int err_col) {   /* 'x' */
    if (the_ch == '\'')
        error(err_line, err_col, "gettok: empty character constant");
    if (the_ch == '\\') {
        next_ch();
        if (the_ch == 'n')
            n = 10;
        else if (the_ch == '\\')
            n = '\\';
        else error(err_line, err_col, "gettok: unknown escape sequence \\%c", the_ch);
    }
    if (next_ch() != '\'')
        error(err_line, err_col, "multi-character constant");
    next_ch();
    return (tok_s){tk_Integer, err_line, err_col, {n}};
}
 
static tok_s div_or_cmt(int err_line, int err_col) { /* process divide or comments */
    if (the_ch != '*')
        return (tok_s){tk_Div, err_line, err_col, {0}};
 
    /* comment found */
    next_ch();
    for (;;) {
        if (the_ch == '*') {
            if (next_ch() == '/') {
                next_ch();
                return gettok();
            }
        } else if (the_ch == EOF)
            error(err_line, err_col, "EOF in comment");
        else
            next_ch();
    }
}
 
static tok_s string_lit(int start, int err_line, int err_col) { /* "st" */
    da_rewind(text);
 
    while (next_ch() != start) {
        if (the_ch == '\n') error(err_line, err_col, "EOL in string");
        if (the_ch == EOF)  error(err_line, err_col, "EOF in string");
        da_append(text, (char)the_ch);
    }
    da_append(text, '\0');
 
    next_ch();
    return (tok_s){tk_String, err_line, err_col, {.text=text}};
}
 
static int kwd_cmp(const void *p1, const void *p2) {
    return strcmp(*(char **)p1, *(char **)p2);
}
 
static TokenType get_ident_type(const char *ident) {
    static struct {
        const char *s;
        TokenType sym;
    } kwds[] = {
        {"begin",  tk_Begin},
        {"end",    tk_End},
        {"loop", tk_Loop},
        {"while",  tk_While},
        {"void", tk_Void},
        {"exit",  tk_Exit},
        {"getter",    tk_Getter},
        {"outter", tk_Outter},
        {"main",  tk_Main},
        {"if", tk_If},
        {"then",    tk_Then},
        {"assign", tk_Assign},
        {"data",  tk_Data},
        {"proc", tk_Proc},
    }, *kwp;
 
    return (kwp = bsearch(&ident, kwds, NELEMS(kwds), sizeof(kwds[0]), kwd_cmp)) == NULL ? tk_Ident : kwp->sym;
}
 
static tok_s ident_or_int(int err_line, int err_col) {
    int n, is_number = true;
 
    da_rewind(text);
    while (isalnum(the_ch) || the_ch == '_') {
        da_append(text, (char)the_ch);
        if (!isdigit(the_ch))
            is_number = false;
        next_ch();
    }
    if (da_len(text) == 0)
        error(err_line, err_col, "gettok: unrecognized character (%d) '%c'\n", the_ch, the_ch);
    da_append(text, '\0');
    if (isdigit(text[0])) {
        if (!is_number)
            error(err_line, err_col, "invalid number: %s\n", text);
        n = strtol(text, NULL, 0);
        if (n == LONG_MAX && errno == ERANGE)
            error(err_line, err_col, "Number exceeds maximum value");
        return (tok_s){tk_Integer, err_line, err_col, {n}};
    }
    return (tok_s){get_ident_type(text), err_line, err_col, {.text=text}};
}
 
static tok_s follow(int expect, TokenType ifyes, TokenType ifno, int err_line, int err_col) {   /* look ahead for '>=', etc. */
    if (the_ch == expect) {
        next_ch();
        return (tok_s){ifyes, err_line, err_col, {0}};
    }
    if (ifno == tk_EOI)
        error(err_line, err_col, "follow: unrecognized character '%c' (%d)\n", the_ch, the_ch);
    return (tok_s){ifno, err_line, err_col, {0}};
}
 
tok_s gettok(void) {            /* return the token type */
    /* skip white space */
    while (isspace(the_ch))
        next_ch();
    int err_line = line;
    int err_col  = col;
    switch (the_ch) {
        case '{':  next_ch(); return (tok_s){tk_Lbrace, err_line, err_col, {0}};
        case '}':  next_ch(); return (tok_s){tk_Rbrace, err_line, err_col, {0}};
        case '(':  next_ch(); return (tok_s){tk_Lparent, err_line, err_col, {0}};
        case ')':  next_ch(); return (tok_s){tk_Rparent, err_line, err_col, {0}};
        case '[':  next_ch(); return (tok_s){tk_Lbracket, err_line, err_col, {0}};
        case ']':  next_ch(); return (tok_s){tk_Rbracket, err_line, err_col, {0}};
        case '+':  next_ch(); return (tok_s){tk_Add, err_line, err_col, {0}};
        case '-':  next_ch(); return (tok_s){tk_Subtract, err_line, err_col, {0}};
        case '*':  next_ch(); return (tok_s){tk_Multiply, err_line, err_col, {0}};
        case '%':  next_ch(); return (tok_s){tk_Modulus, err_line, err_col, {0}};
        case ';':  next_ch(); return (tok_s){tk_Semicolon, err_line, err_col, {0}};
        case ',':  next_ch(); return (tok_s){tk_Comma,err_line, err_col, {0}};
        case '.':  next_ch(); return (tok_s){tk_Dot,err_line, err_col, {0}};
        case '/':  next_ch(); return div_or_cmt(err_line, err_col);
        case '\'': next_ch(); return char_lit(the_ch, err_line, err_col);
        case ':':  next_ch(); return follow('=', tk_Assign2, tk_Colon, err_line, err_col);
        case '=':  next_ch(); return follow('>', tk_GreaterEq, tk_Assign1, err_line, err_col);
        case '=':  next_ch(); return follow('<', tk_LessEq,  tk_Assign1, err_line, err_col);
        case '=':  next_ch(); return follow('=', tk_Compare, tk_Assign1, err_line, err_col);
        case '"' : return string_lit(the_ch, err_line, err_col);
        default:   return ident_or_int(err_line, err_col);
        case EOF:  return (tok_s){tk_EOI, err_line, err_col, {0}};
    }
}
 
void run(void) {    /* tokenize the given input */
    tok_s tok;
    do {
        tok = gettok();
        fprintf(dest_fp, "%5d  %5d %.15s",
            tok.err_ln, tok.err_col,
            &"End_of_input    Op_assign1      Op_greatereq    Op_lesseq       Op_compare      "
             "Op_assign2      Op_add          Op_subtract     Op_multiply     Op_divide       "
             "Op_modulus      Delim_colin     Delim_semicol   Delim_dot       Delim_Comma     "
             "Delim_LParent   Delim_RParent   Delim_RBrace    Delim_LBrace    Delim_RBracket  "
             "Delim_LBracket  Keyword_begin   Keyword_end     Keyword_loop    Keyword_while   "
             "Keyword_void    Keyword_exit    Keyword_getter  Keyword_outter  Keyword_main    "
             "Keyword_if      Keyword_then    Keyword_assign  Keyword_data    Keyword_proc    "
             "Identifier      Integer         String          "
            [tok.tok * 16]);
        if (tok.tok == tk_Integer)     fprintf(dest_fp, "  %4d",   tok.n);
        else if (tok.tok == tk_Ident)  fprintf(dest_fp, " %s",     tok.text);
        else if (tok.tok == tk_String) fprintf(dest_fp, " \"%s\"", tok.text);
        fprintf(dest_fp, "\n");
    } while (tok.tok != tk_EOI);
    if (dest_fp != stdout)
        fclose(dest_fp);
}
 
void init_io(FILE **fp, FILE *std, const char mode[], const char fn[]) {
    if (fn[0] == '\0')
        *fp = std;
    else if ((*fp = fopen(fn, mode)) == NULL)
        error(0, 0, "Can't open %s\n", fn);
}
 
int main(int argc, char *argv[]) {
    init_io(&source_fp, stdin,  "r",  argc > 1 ? argv[1] : "");
    init_io(&dest_fp,   stdout, "wb", argc > 2 ? argv[2] : "");
    run();
    return 0;
}
