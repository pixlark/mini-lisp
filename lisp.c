/*
 * miniature Lisp implementation
 */

#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __GNUC__
#define NORETURN __attribute__((noreturn))
#else
#define NORETURN
#endif

void NORETURN fatal(const char* message, ...) {
    va_list args;
    va_start(args, message);
    vfprintf(stderr, message, args);
    fprintf(stderr, "\n");
    va_end(args);
    exit(1);
}

/*
 * string interner
 */

typedef struct Interner_Node {
    const char* string;
    struct Interner_Node* next;
} Interner_Node;

const char* QUOTE  = "quote",
          * ATOM   = "atom",
          * EQ     = "eq",
          * CAR    = "car",
          * CDR    = "cdr",
          * CONS   = "cons",
          * COND   = "cond",
          * T      = "t",
          * LAMBDA = "lambda",
          * LABEL  = "label";

typedef struct {
    Interner_Node* head;
} Interner;

void internVerbatim(Interner* interner, const char* string) {
    Interner_Node* node = (Interner_Node*) malloc(sizeof(Interner_Node));
    node->string = string; // STORES `string` DIRECTLY!
    node->next = interner->head;

    interner->head = node;
}

Interner createInterner() {
    Interner interner;
    interner.head = NULL;

    internVerbatim(&interner, QUOTE);
    internVerbatim(&interner, ATOM);
    internVerbatim(&interner, EQ);
    internVerbatim(&interner, CAR);
    internVerbatim(&interner, CDR);
    internVerbatim(&interner, CONS);
    internVerbatim(&interner, COND);
    internVerbatim(&interner, T);
    internVerbatim(&interner, LAMBDA);
    internVerbatim(&interner, LABEL);

    return interner;
}

// Symbols in Lisp are case-insensitive
bool stringsEqual(const char* a, const char* b) {
    size_t a_length = strlen(a);
    size_t b_length = strlen(b);
    if (a_length != b_length) {
        return false;
    }

    for (size_t i = 0; i < a_length; i++) {
        if (tolower(a[i]) != tolower(b[i])) {
            return false;
        }
    }

    return true;
}

const char* allocSymbol(const char* string) {
    // Canonicalize to uppercase!
    size_t length = strlen(string);
    char* s = (char*) malloc(sizeof(char) * (length + 1));
    s[length] = '\0';
    for (size_t i = 0; i < length; i++) {
        s[i] = tolower(string[i]);
    }
    return s;
}

const char* intern(Interner* interner, const char* string) {
    // Check if string is already interned
    Interner_Node* iter = interner->head;
    while (iter != NULL) {
        if (stringsEqual(string, iter->string)) {
            return iter->string;
        }
        iter = iter->next;
    }

    // Add new internment for string
    Interner_Node* internment = (Interner_Node*) malloc(sizeof(Interner_Node));
    internment->next = interner->head;
    internment->string = strdup(string);
    interner->head = internment;
    return internment->string;
}

/*
 * extremely simple lexer
 */

typedef struct {
    size_t cursor;
    size_t length;
    const char* source;
    Interner interner;
} Lexer;

Lexer createLexer(const char* source) {
    Lexer lexer;
    lexer.cursor = 0;
    lexer.length = strlen(source);
    lexer.source = source;
    lexer.interner = createInterner();
    return lexer;
}

bool lexerEnd(const Lexer* lexer) {
    return lexer->cursor >= lexer->length;
}

typedef enum {
    TOKEN_EOF,
    TOKEN_OPEN_PAREN,
    TOKEN_CLOSE_PAREN,
    TOKEN_SYMBOL
} Token_Type;

typedef struct {
    Token_Type type;
    const char* symbol;
} Token;

Token createToken(Token_Type type) {
    Token t;
    t.type = type;
    return t;
}

bool isSymbolChar(char c) {
    // enough for now
    return isalpha(c) 
        || c == '-' || c == '+' 
        || c == '?' || c == '!';
}

Token lexerNext(Lexer* lexer) {
    if (lexerEnd(lexer)) {
        return createToken(TOKEN_EOF);
    }

    while (isspace(lexer->source[lexer->cursor])) {
        lexer->cursor++;
        if (lexerEnd(lexer)) {
            return createToken(TOKEN_EOF);
        }
    }

    if (isSymbolChar(lexer->source[lexer->cursor])) {
        // TODO(brooke): make this variably sized (surely 
        // we can make a simple char vector implementation)
        enum { MAX_SYMBOL_SIZE = 64 };
        char symbol[MAX_SYMBOL_SIZE];
        size_t symbol_cursor = 0;
        while (symbol_cursor < MAX_SYMBOL_SIZE - 1) {
            char c = lexer->source[lexer->cursor];
            if (!isSymbolChar(c) && !isdigit(c)) {
                break;
            }
            lexer->cursor++;
            symbol[symbol_cursor++] = c;
        }
        symbol[symbol_cursor] = '\0';

        Token t = createToken(TOKEN_SYMBOL);
        t.symbol = intern(&lexer->interner, symbol);
        return t;
    }

    char c = lexer->source[lexer->cursor++];
    switch (c) {
    case '(':
        return createToken(TOKEN_OPEN_PAREN);
    case ')':
        return createToken(TOKEN_CLOSE_PAREN);
    default:
        // TODO(brooke): make fatal varargs so we can say what character
        printf("%zu %zu\n", lexer->cursor, lexer->length);
        fatal("Encountered unexpected character %c (0x%x).", c, c);
    }
}

/*
 * parsing s-expressions
 */

typedef enum {
    // Atoms
    CAR_OF_SYMBOL,
    // Nested cons
    CAR_OF_CONS
} Car_Type;

typedef struct Cons {
    // car
    Car_Type car_type;
    union {
        const char*  car_symbol;
        struct Cons* car_cons;
    };
    // cdr
    struct Cons* cdr;
} Cons;

void dumpCons(const Cons* cons);

void dumpCons_inner(const Cons* cons) {
    if (cons == NULL) {
        printf(") ");
        return;
    }

    if (cons->car_type == CAR_OF_SYMBOL) {
        printf("%s ", cons->car_symbol);
    } else if (cons->car_type == CAR_OF_CONS) {
        dumpCons(cons->car_cons);
    }

    dumpCons_inner(cons->cdr);
}

void dumpCons(const Cons* cons) {
    printf("( ");
    dumpCons_inner(cons);
}

typedef struct {
    Lexer* lexer;
} Parser;

Parser createParser(Lexer* lexer) {
    Parser parser;
    parser.lexer = lexer;
    return parser;
}

void parserChomp(Parser* parser, Token_Type type) {
    Token t = lexerNext(parser->lexer);
    if (t.type != type) {
        fatal("Expected token type %d, got token type %d.", type, t.type);
    }
}

// To make the parser simpler, parseCons parses a cons 
// *excluding* the first open parentheses.
Cons* parseCons(Parser* parser) {
    Token t = lexerNext(parser->lexer);
    if (t.type == TOKEN_CLOSE_PAREN) {
        // This means we've found essentially a `(quote ())`
        return NULL;
    }

    Cons* cons = (Cons*) malloc(sizeof(Cons));

    // Car can be either a symbol or another cons
    if (t.type == TOKEN_SYMBOL) {
        cons->car_type = CAR_OF_SYMBOL;
        cons->car_symbol = t.symbol;
    } else if (t.type == TOKEN_OPEN_PAREN) {
        cons->car_type = CAR_OF_CONS;
        cons->car_cons = parseCons(parser);
    } else {
        fatal("Expected symbol or ( -- instead, got token type %d.", t.type);
    }

    cons->cdr = parseCons(parser);

    return cons;
}

bool parserNext(Parser* parser, Cons** ret) {
    Token t = lexerNext(parser->lexer);
    if (t.type == TOKEN_EOF) {
        return false;
    } else if (t.type != TOKEN_OPEN_PAREN) {
        fatal("Expected ( -- instead, got token type %d.", t.type);
    }

    *ret = parseCons(parser);
    return true;
}

/*
 * the actual tree-walk interpreter
 */

// TODO(brooke): Is there a neat way to unify Value with the car of a Cons? 
//                They perform essentially the same function, and keeping 
//                them separate means we have to constantly convert between 
//                the two formats.

typedef enum {
    VALUE_SYMBOL,
    VALUE_CONS,
    VALUE_FUNCTION
} Value_Type;

struct Environment;

typedef struct {
    const char** parameters;
    size_t parameter_count;
    Cons* body;
    // TODO(brooke): once garbage collection is happening, this needs to get collected too
    struct Environment* closure;
} Function;

typedef struct {
    Value_Type type;
    union {
        const char* symbol;
        Cons* cons;
        Function function;
    };
} Value;

Value valueSymbol(const char* symbol) {
    Value val;
    val.type = VALUE_SYMBOL;
    val.symbol = symbol;
    return val;
}

Value valueCons(Cons* cons) {
    Value val;
    val.type = VALUE_CONS;
    val.cons = cons;
    return val;
}

Value carToValue(Cons* cons) {
    if (cons->car_type == CAR_OF_SYMBOL) {
        return valueSymbol(cons->car_symbol);
    } else if (cons->car_type == CAR_OF_CONS) {
        return valueCons(cons->car_cons);
    } else {
        fatal("Malformed cons passed to carToValue.");
    }
}

Value boolToValue(bool b) {
    if (b) {
        return valueSymbol(T);
    } else {
        return valueCons(NULL);
    }
}

bool isValueTruthy(Value value) {
    return value.type == VALUE_SYMBOL && value.symbol == T;
}

typedef struct Binding {
    const char* symbol;
    Value value;
} Binding;

typedef struct Environment {
    Binding* bindings;
    size_t   binding_count;
} Environment;

Environment createEmptyEnvironment() {
    Environment env;
    env.bindings = NULL;
    env.binding_count = 0;
    return env;
}

bool lookupBinding(const Environment* env, const char* symbol, Value* ret_value) {
    for (size_t i = 0; i < env->binding_count; i++) {
        size_t ir = env->binding_count - i - 1; // Search in reverse-order
        if (env->bindings[ir].symbol == symbol) {
            *ret_value = env->bindings[ir].value;
            return true;
        }
    }
    return false;
}

void validateArgumentCount(const char* name, Cons* cons, size_t count) {
    size_t running_count = count;

    Cons* cdr = cons->cdr;
    while (cdr != NULL) {
        // Getting inside this loop body means that there are more arguments
        // to come. `running_count` indicates how many we expect. If it's zero,
        // there's too many.
        if (running_count == 0) {
            fatal("%s requires %zu arguments.\n", name, count);
        }
        cdr = cdr->cdr;
        running_count--;
    }

    // Leaving the loop means we've walked through all the arguments. So,
    // if we were expecting any more we'll catch that here.
    if (running_count != 0) {
        fatal("%s requires %zu arguments.\n", name, count);
    }
}

Value treeWalk(Value input, Environment env) {
    // Anything that's not a Cons is an atom,
    // and atoms evaluate to themselves.
    if (input.type != VALUE_CONS) {
        if (input.type == VALUE_SYMBOL) {
            // Symbols evaluate to their bindings
            Value bound;
            if (!lookupBinding(&env, input.symbol, &bound)) {
                fatal("Use of unbound variable %s.", input.symbol);
            }
            return bound;
        } else if (input.type == VALUE_FUNCTION) {
            // Functions evaluate to themselves
            return input;
        } else {
            fatal("treeWalk was passed a malformed value.");
        }
        return input;
    }

    // If the input *is* a cons, that's when evaluation
    // gets interesting:
    Cons* cons = input.cons;
    
    if (cons->car_symbol == QUOTE) {
        validateArgumentCount(QUOTE, cons, 1);

        Cons* cdr = cons->cdr;
        if (cdr->car_type == CAR_OF_SYMBOL) {
            return valueSymbol(cdr->car_symbol);
        } else {
            return valueCons(cdr->car_cons);
        }
    } else if (cons->car_symbol == ATOM) {
        validateArgumentCount(ATOM, cons, 1);

        Value parameter = carToValue(cons->cdr);

        // EVALUATE the argument
        Value argument = treeWalk(parameter, env);

        // The predicate `atom` returns t if the argument is:
        //     (1) a symbol
        //  or (2) the empty list
        // otherwise it returns the empty list
        if (argument.type == VALUE_SYMBOL ||
           (argument.type == VALUE_CONS && argument.cons == NULL)) {
            return boolToValue(true);
        } else {
            return boolToValue(false);
        }
    } else if (cons->car_symbol == EQ) { 
        validateArgumentCount(EQ, cons, 2);
        
        Value parameter_a = carToValue(cons->cdr);
        Value parameter_b = carToValue(cons->cdr->cdr);

        // EVALUATE the arguments
        Value argument_a = treeWalk(parameter_a, env);
        Value argument_b = treeWalk(parameter_b, env);

        // The predicate `eq` returns t if the arguments are:
        //     (1) both symbols and both the same symbol
        //  or (2) both lists and both the empty list
        if (argument_a.type == VALUE_SYMBOL && argument_b.type == VALUE_SYMBOL) {
            return boolToValue(argument_a.symbol == argument_b.symbol);
        } else if (argument_a.type == VALUE_CONS && argument_b.type == VALUE_CONS) {
            return boolToValue(argument_a.cons == NULL && argument_b.cons == NULL);
        }
        return boolToValue(false);
    } else if (cons->car_symbol == CAR) {
        validateArgumentCount(CAR, cons, 1);

        Value parameter = carToValue(cons->cdr);

        // EVALUATE the argument
        Value argument = treeWalk(parameter, env);

        if (argument.type != VALUE_CONS) {
            fatal("car expects a list as its argument, but it got a non-list.");
        }
        if (argument.cons == NULL) {
            fatal("car expects a non-empty list as its argument, but it got ().");
        }
        return carToValue(argument.cons);
    } else if (cons->car_symbol == CDR) {
        validateArgumentCount(CDR, cons, 1);

        Value parameter = carToValue(cons->cdr);

        // EVALUATE the argument
        Value argument = treeWalk(parameter, env);

        if (argument.type != VALUE_CONS) {
            fatal("cdr expects a list as its argument, but it got a non-list.");
        }
        if (argument.cons == NULL) {
            fatal("cdr expects a non-empty list as its argument, but it got ().");
        }
        return valueCons(argument.cons->cdr);
    } else if (cons->car_symbol == CONS) {
        validateArgumentCount(CONS, cons, 2);
        
        Value parameter_a = carToValue(cons->cdr);
        Value parameter_b = carToValue(cons->cdr->cdr);

        // EVALUATE the arguments
        Value argument_a = treeWalk(parameter_a, env);
        Value argument_b = treeWalk(parameter_b, env);

        // cons expects nothing from its first argument - it can have any type
        // TODO(brooke): Technically, cons should allow *anything* for its second argument (dotted pairs, anyone?)
        //               For right now, it's going to require a list, however.
        if (argument_b.type != VALUE_CONS) {
            fatal("cons expects a list as its second argument, but it got a symbol.");
        }

        // NOTE: This is the only built-in function that performs an allocation!
        // Which TODO(brooke) are currently not garbage-collected.
        Cons* new_cons = (Cons*) malloc(sizeof(Cons));
        if (argument_a.type == VALUE_SYMBOL) {
            new_cons->car_type = CAR_OF_SYMBOL;
            new_cons->car_symbol = argument_a.symbol;
        } else if (argument_a.type == VALUE_CONS) {
            new_cons->car_type = CAR_OF_CONS;
            new_cons->car_cons = argument_a.cons;
        } else {
            // TODO(brooke): So, right now you can't explicitly cons a lambda, because our 
            //               Cons structure has no room for lambdas. If/when we unify car values 
            //               with the Value struct, this problem will go away. For now, it's 
            //               just a little bit of an annoyance.
            fatal("First argument of a cons evaluated to something malformed.");
        }
        new_cons->cdr = argument_b.cons;
        return valueCons(new_cons);
    } else if (cons->car_symbol == COND) {
        // Evaluate arguments one at a time
        Cons* iter_cons = cons;
        while (true) {
            Cons* cdr = iter_cons->cdr;
            if (cdr == NULL) {
                break;
            }

            Value parameter = carToValue(cdr);

            // TODO(brooke): wow this is fucking ugly. please make it look pretty.
            if (parameter.type != VALUE_CONS ||
                parameter.cons == NULL ||
                parameter.cons->cdr == NULL ||
                parameter.cons->cdr->cdr != NULL) {
                fatal("cond arguments follow the format (<predicate> <value>).");
            }

            // First, EVALUATE the predicate.
            Value predicate = treeWalk(carToValue(parameter.cons), env);
            if (isValueTruthy(predicate)) {
                // If it evaluates to t, then EVALUATE the result and return.
                return treeWalk(carToValue(parameter.cons->cdr), env);
            }

            iter_cons = cdr;
        }
        fatal("No branch in cond evaluated to t.");
    } else if (cons->car_symbol == LAMBDA) {
        const char* format_error = "lambda follows the format (lambda (<parameter> ...) <expr> ...)";

        if (cons == NULL || cons->cdr == NULL) {
            fatal(format_error);
        }

        Value parameter_list = carToValue(cons->cdr);
        if (parameter_list.type != VALUE_CONS) {
            fatal(format_error);
        }

        // Collect parameters
        size_t parameter_count = 0;
        {
            Cons* iter = parameter_list.cons;
            while (iter != NULL) {
                if (iter->car_type != CAR_OF_SYMBOL) {
                    fatal(format_error);
                }
                parameter_count++;
                iter = iter->cdr;
            }
        }
        const char** parameter_arr = (const char**) malloc(sizeof(char*) * parameter_count);
        {
            Cons* iter = parameter_list.cons;
            for (size_t i = 0; i < parameter_count; i++) {
                parameter_arr[i] = iter->car_symbol;

                iter = iter->cdr;
            }
        }

        Value function;
        function.type = VALUE_FUNCTION;
        function.function.parameter_count = parameter_count;
        function.function.parameters = parameter_arr;
        function.function.body = (cons->cdr->cdr);
        function.function.closure = (Environment*) malloc(sizeof(Environment));
        *function.function.closure = createEmptyEnvironment();
        return function;
    } else if (cons->car_symbol == LABEL) {
        const char* format_error = "label follows the format (label <symbol> <function>)";
        validateArgumentCount(LABEL, cons, 2);

        Value label_param = carToValue(cons->cdr);
        Value function_param = carToValue(cons->cdr->cdr);

        if (label_param.type != VALUE_SYMBOL) {
            fatal(format_error);
        }
        const char* label = label_param.symbol;

        // EVALUATE the function
        Value function = treeWalk(function_param, env);
        
        if (function.type != VALUE_FUNCTION) {
            fatal(format_error);
        }

        // Create a new environment with the extra binding.
        // TODO(brooke): nested labels break here
        Environment new_env;
        new_env.binding_count = 1;
        new_env.bindings = (Binding*) malloc(sizeof(Binding));

        Binding binding;
        binding.symbol = label;
        binding.value = function;

        *new_env.bindings = binding;
        *function.function.closure = new_env;

        // Return
        return function;
    } else {
        Value to_call = treeWalk(carToValue(cons), env);
        if (to_call.type != VALUE_FUNCTION) {
            fatal("Tried to call a non-function.");
        }
        size_t argument_count = 0;
        {
            Cons* iter = cons->cdr;
            while (iter != NULL) {
                argument_count++;
                iter = iter->cdr;
            }
            if (argument_count != to_call.function.parameter_count) {
                fatal("Tried to call a function with the wrong number of parameters.");
            }
        }

        // Call this function on the arguments
        Environment new_env;
        new_env.binding_count = env.binding_count 
                              + to_call.function.parameter_count 
                              + to_call.function.closure->binding_count;
        new_env.bindings = (Binding*) malloc(sizeof(Binding) * new_env.binding_count);

        // Copy in the surrounding bindings
        memcpy(new_env.bindings, env.bindings, sizeof(Binding) * env.binding_count);

        // Evaluate the arguments and add them to the new environment
        Cons* iter = cons->cdr;
        for (size_t i = 0; i < to_call.function.parameter_count; i++) {
            Binding binding;
            binding.symbol = to_call.function.parameters[i];
            
            Value argument = treeWalk(carToValue(iter), env);
            binding.value = argument;

            new_env.bindings[i + env.binding_count] = binding;
            iter = iter->cdr;
        }

        // Add the closure bindings to the environment
        memcpy(new_env.bindings + (env.binding_count + to_call.function.parameter_count),
               to_call.function.closure->bindings,
               sizeof(Binding) * to_call.function.closure->binding_count);

        // Actually run the function
        Cons* body_iter = to_call.function.body;
        Value result = valueCons(NULL);
        while (body_iter != NULL) {
            result = treeWalk(carToValue(body_iter), new_env);
            body_iter = body_iter->cdr;
        }

        free(new_env.bindings);
        return result;
    }
}

int main(int argc, const char** argv) {
    if (argc != 2) {
        printf("usage: ./lisp <source file>\n");
        return 1;
    }
    const char* source_path = argv[1];

    FILE* source_file = fopen(source_path, "r");
    if (source_file == NULL) {
        printf("no such source file at '%s'\n", source_path);
        return 1;
    }

    // Keep things simple for now.
    // TODO(brooke): fix later!
    enum { SOURCE_MAX_SIZE = 512 };
    char source[SOURCE_MAX_SIZE];
    int result = fread(source, sizeof(char), SOURCE_MAX_SIZE - 1, source_file);
    if (result < SOURCE_MAX_SIZE - 1 && ferror(source_file)) {
        printf("error reading source file at '%s'\n", source_path);
        return 1;
    }
    source[result] = '\0';

    Lexer lexer = createLexer(source);
    Parser parser = createParser(&lexer);
    Cons* cons;
    Environment outer_environment = createEmptyEnvironment();
    while (parserNext(&parser, &cons)) {
        // Each cons here is a top-level list
        Value result = treeWalk(valueCons(cons), outer_environment);
        if (result.type == VALUE_SYMBOL) {
            printf("%s\n", result.symbol);
        } else if (result.type == VALUE_CONS) {
            dumpCons(result.cons);
            printf("\n");
        } else if (result.type == VALUE_FUNCTION) {
            printf("<lambda - params: [");
            for (size_t i = 0; i < result.function.parameter_count; i++) {
                printf("%s", result.function.parameters[i]);
                if (i < result.function.parameter_count - 1) {
                    printf(" ");
                }
            }
            printf("] - body: ");
            dumpCons(result.function.body);
            printf(">\n");
        } else {
            fatal("Malformed result.");
        }
    }
}
