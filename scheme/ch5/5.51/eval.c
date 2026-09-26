/* The explicit-control evaluator of section 5.4 written in C.
 *
 * The registers are C variables and the controller is one function whose
 * labels are C labels. The continue register holds a Label, and
 * go_to_continue jumps to the C label it names. Procedure application and
 * sequences do not save anything before their last step, so the evaluator
 * runs iterative processes in constant space. */

#include "eval.h"

#include "environment.h"
#include "error.h"
#include "memory.h"
#include "primitives.h"

typedef enum {
    DONE,
    EV_APPL_DID_OPERATOR,
    EV_APPL_ACCUMULATE_ARG,
    EV_APPL_ACCUM_LAST_ARG,
    EV_SEQUENCE_CONTINUE,
    EV_IF_DECIDE,
    EV_COND_DECIDE,
    EV_ASSIGNMENT_1,
    EV_DEFINITION_1
} Label;

static struct {
    Value exp, env, val, cont, proc, argl, unev;
} reg;

static struct {
    Value quote, set, define, if_, lambda, begin, cond, let, else_, ok;
} symbol;

void init_evaluator(void)
{
    add_root(&reg.exp);
    add_root(&reg.env);
    add_root(&reg.val);
    add_root(&reg.cont);
    add_root(&reg.proc);
    add_root(&reg.argl);
    add_root(&reg.unev);

    symbol.quote = intern("quote");
    symbol.set = intern("set!");
    symbol.define = intern("define");
    symbol.if_ = intern("if");
    symbol.lambda = intern("lambda");
    symbol.begin = intern("begin");
    symbol.cond = intern("cond");
    symbol.let = intern("let");
    symbol.else_ = intern("else");
    symbol.ok = intern("ok");
}
