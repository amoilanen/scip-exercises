//! The explicit-control evaluator of section 5.4 written in Rust.
//!
//! The registers are fields of `Registers` and the controller is one loop
//! over the labels of the book's controller: each step executes the
//! instructions after a label and yields the label to go to next. The
//! continue register holds a `Label`, and going to it is going to the label
//! it holds. Procedure application and sequences do not save anything
//! before their last step, so the evaluator runs iterative processes in
//! constant space.

use crate::error::Result;
use crate::machine::Machine;
use crate::object::{Texts, Value};

/// The labels of the controller.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Label {
    Done,
    EvalDispatch,
    EvApplication,
    EvApplDidOperator,
    EvApplOperandLoop,
    EvApplAccumulateArg,
    EvApplLastArg,
    EvApplAccumLastArg,
    ApplyDispatch,
    EvSequence,
    EvSequenceContinue,
    EvIf,
    EvIfDecide,
    EvCond,
    EvCondLoop,
    EvCondDecide,
    EvAssignment,
    EvAssignment1,
    EvDefinition,
    EvDefinition1,
}

pub struct Registers {
    pub exp: Value,
    pub env: Value,
    pub val: Value,
    pub cont: Value,
    pub proc: Value,
    pub argl: Value,
    pub unev: Value,
}

impl Registers {
    pub fn new() -> Registers {
        Registers {
            exp: Value::EmptyList,
            env: Value::EmptyList,
            val: Value::EmptyList,
            cont: Value::Label(Label::Done),
            proc: Value::EmptyList,
            argl: Value::EmptyList,
            unev: Value::EmptyList,
        }
    }

    /// The registers as roots of the garbage collector.
    pub fn all_mut(&mut self) -> [&mut Value; 7] {
        [
            &mut self.exp,
            &mut self.env,
            &mut self.val,
            &mut self.cont,
            &mut self.proc,
            &mut self.argl,
            &mut self.unev,
        ]
    }
}

/// The symbols of the special forms, and ok, the value of definitions and
/// assignments.
#[derive(Clone, Copy)]
pub struct Keywords {
    quote: Value,
    set: Value,
    define: Value,
    if_: Value,
    lambda: Value,
    begin: Value,
    cond: Value,
    let_: Value,
    else_: Value,
    ok: Value,
}

impl Keywords {
    pub fn new(texts: &mut Texts) -> Keywords {
        Keywords {
            quote: texts.intern("quote"),
            set: texts.intern("set!"),
            define: texts.intern("define"),
            if_: texts.intern("if"),
            lambda: texts.intern("lambda"),
            begin: texts.intern("begin"),
            cond: texts.intern("cond"),
            let_: texts.intern("let"),
            else_: texts.intern("else"),
            ok: texts.intern("ok"),
        }
    }
}

fn is_self_evaluating(exp: Value) -> bool {
    matches!(
        exp,
        Value::Fixnum(_) | Value::Flonum(_) | Value::Str(_) | Value::Boolean(_)
    )
}

/*** Syntax ***/

impl Machine {
    fn is_tagged_list(&self, exp: Value, tag: Value) -> bool {
        exp.is_pair() && self.cell_car(exp) == tag
    }

    fn make_lambda(&mut self, parameters: Value, body: Value) -> Result<Value> {
        let rest = self.cons(parameters, body)?;
        self.cons(self.keywords.lambda, rest)
    }

    fn definition_variable(&self, exp: Value) -> Result<Value> {
        let target = self.cadr(exp)?;
        if target.is_symbol() {
            Ok(target)
        } else {
            self.car(target)
        }
    }

    fn definition_value(&mut self, exp: Value) -> Result<Value> {
        let target = self.cadr(exp)?;
        if target.is_symbol() {
            return self.caddr(exp);
        }
        let (parameters, body) = (self.cdr(target)?, self.cddr(exp)?);
        self.make_lambda(parameters, body)
    }

    fn reverse_in_place(&mut self, list: Value) -> Result<Value> {
        let mut reversed = Value::EmptyList;
        let mut list = list;
        while list.is_pair() {
            let rest = self.cell_cdr(list);
            self.set_cdr(list, reversed)?;
            reversed = list;
            list = rest;
        }
        Ok(reversed)
    }

    /// (let ((v e) ...) body ...) is ((lambda (v ...) body ...) e ...)
    fn let_to_combination(&mut self, exp: Value) -> Result<Value> {
        let bindings = self.cadr(exp)?;
        let exp = self.protect(exp)?;
        let bindings = self.protect(bindings)?;
        let variables = self.protect(Value::EmptyList)?;
        let operands = self.protect(Value::EmptyList)?;
        while self.protected(bindings).is_pair() {
            let variable = self.caar(self.protected(bindings))?;
            let list = self.cons(variable, self.protected(variables))?;
            self.set_protected(variables, list);
            let operand = self.cadr(self.car(self.protected(bindings))?)?;
            let list = self.cons(operand, self.protected(operands))?;
            self.set_protected(operands, list);
            let rest = self.cell_cdr(self.protected(bindings));
            self.set_protected(bindings, rest);
        }
        let parameters = self.reverse_in_place(self.protected(variables))?;
        let body = self.cddr(self.protected(exp))?;
        let lambda = self.make_lambda(parameters, body)?;
        let arguments = self.reverse_in_place(self.protected(operands))?;
        let combination = self.cons(lambda, arguments)?;
        self.unprotect(4);
        Ok(combination)
    }

    fn make_procedure(&mut self, lambda: Value, env: Value) -> Result<Value> {
        self.make_cell(Value::Procedure, lambda, env)
    }

    fn procedure_parameters(&self, procedure: Value) -> Result<Value> {
        self.cadr(self.cell_car(procedure))
    }

    fn procedure_body(&self, procedure: Value) -> Result<Value> {
        self.cddr(self.cell_car(procedure))
    }

    fn procedure_environment(&self, procedure: Value) -> Value {
        self.cell_cdr(procedure)
    }
}

/*** The controller ***/

impl Machine {
    /// Clears the registers and the stack after an error.
    pub fn reset_evaluator(&mut self) {
        self.reg = Registers::new();
        self.stack.clear();
        self.protected.clear();
    }

    /// Runs the explicit-control evaluator on exp in env and returns the
    /// value.
    pub fn evaluate(&mut self, exp: Value, env: Value) -> Result<Value> {
        self.reg.exp = exp;
        self.reg.env = env;
        self.reg.cont = Value::Label(Label::Done);
        self.execute()?;
        Ok(self.reg.val)
    }

    /// (goto (reg continue))
    fn go_to_continue(&self) -> Result<Label> {
        match self.reg.cont {
            Value::Label(label) => Ok(label),
            other => Err(self.error("Not a label:", other)),
        }
    }

    fn execute(&mut self) -> Result<()> {
        let keywords = self.keywords;
        let mut label = Label::EvalDispatch;
        loop {
            label = match label {
                Label::Done => return Ok(()),

                Label::EvalDispatch => {
                    let exp = self.reg.exp;
                    if is_self_evaluating(exp) {
                        self.reg.val = exp;
                        self.go_to_continue()?
                    } else if exp.is_symbol() {
                        self.reg.val = self.lookup_variable_value(exp, self.reg.env)?;
                        self.go_to_continue()?
                    } else if self.is_tagged_list(exp, keywords.quote) {
                        self.reg.val = self.cadr(exp)?;
                        self.go_to_continue()?
                    } else if self.is_tagged_list(exp, keywords.set) {
                        Label::EvAssignment
                    } else if self.is_tagged_list(exp, keywords.define) {
                        Label::EvDefinition
                    } else if self.is_tagged_list(exp, keywords.if_) {
                        Label::EvIf
                    } else if self.is_tagged_list(exp, keywords.lambda) {
                        self.reg.val = self.make_procedure(exp, self.reg.env)?;
                        self.go_to_continue()?
                    } else if self.is_tagged_list(exp, keywords.begin) {
                        self.reg.unev = self.cdr(exp)?;
                        self.save(self.reg.cont)?;
                        Label::EvSequence
                    } else if self.is_tagged_list(exp, keywords.cond) {
                        Label::EvCond
                    } else if self.is_tagged_list(exp, keywords.let_) {
                        self.reg.exp = self.let_to_combination(exp)?;
                        Label::EvApplication
                    } else if exp.is_pair() {
                        Label::EvApplication
                    } else {
                        return Err(self.error("Unknown expression type", exp));
                    }
                }

                Label::EvApplication => {
                    self.save(self.reg.cont)?;
                    self.save(self.reg.env)?;
                    self.reg.unev = self.cdr(self.reg.exp)?;
                    self.save(self.reg.unev)?;
                    self.reg.exp = self.car(self.reg.exp)?;
                    self.reg.cont = Value::Label(Label::EvApplDidOperator);
                    Label::EvalDispatch
                }
                Label::EvApplDidOperator => {
                    self.reg.unev = self.restore();
                    self.reg.env = self.restore();
                    self.reg.argl = Value::EmptyList;
                    self.reg.proc = self.reg.val;
                    if self.reg.unev.is_null() {
                        Label::ApplyDispatch
                    } else {
                        self.save(self.reg.proc)?;
                        Label::EvApplOperandLoop
                    }
                }
                Label::EvApplOperandLoop => {
                    self.save(self.reg.argl)?;
                    self.reg.exp = self.car(self.reg.unev)?;
                    if self.cdr(self.reg.unev)?.is_null() {
                        Label::EvApplLastArg
                    } else {
                        self.save(self.reg.env)?;
                        self.save(self.reg.unev)?;
                        self.reg.cont = Value::Label(Label::EvApplAccumulateArg);
                        Label::EvalDispatch
                    }
                }
                Label::EvApplAccumulateArg => {
                    self.reg.unev = self.restore();
                    self.reg.env = self.restore();
                    self.reg.argl = self.restore();
                    // The arguments are collected in reverse order.
                    self.reg.argl = self.cons(self.reg.val, self.reg.argl)?;
                    self.reg.unev = self.cdr(self.reg.unev)?;
                    Label::EvApplOperandLoop
                }
                Label::EvApplLastArg => {
                    self.reg.cont = Value::Label(Label::EvApplAccumLastArg);
                    Label::EvalDispatch
                }
                Label::EvApplAccumLastArg => {
                    self.reg.argl = self.restore();
                    self.reg.argl = self.cons(self.reg.val, self.reg.argl)?;
                    self.reg.argl = self.reverse_in_place(self.reg.argl)?;
                    self.reg.proc = self.restore();
                    Label::ApplyDispatch
                }

                Label::ApplyDispatch => match self.reg.proc {
                    Value::Primitive(index) => {
                        self.reg.val = self.apply_primitive_procedure(index, self.reg.argl)?;
                        self.reg.cont = self.restore();
                        self.go_to_continue()?
                    }
                    Value::Procedure(_) => {
                        // extend_environment may move the procedure, so
                        // the body is taken from the register afterwards.
                        self.reg.unev = self.procedure_parameters(self.reg.proc)?;
                        self.reg.env = self.procedure_environment(self.reg.proc);
                        self.reg.env =
                            self.extend_environment(self.reg.unev, self.reg.argl, self.reg.env)?;
                        self.reg.unev = self.procedure_body(self.reg.proc)?;
                        Label::EvSequence
                    }
                    procedure => {
                        return Err(self.error("The object is not applicable:", procedure));
                    }
                },

                // The caller has saved continue.
                Label::EvSequence => {
                    self.reg.exp = self.car(self.reg.unev)?;
                    if self.cdr(self.reg.unev)?.is_null() {
                        self.reg.cont = self.restore();
                    } else {
                        self.save(self.reg.unev)?;
                        self.save(self.reg.env)?;
                        self.reg.cont = Value::Label(Label::EvSequenceContinue);
                    }
                    Label::EvalDispatch
                }
                Label::EvSequenceContinue => {
                    self.reg.env = self.restore();
                    self.reg.unev = self.restore();
                    self.reg.unev = self.cdr(self.reg.unev)?;
                    Label::EvSequence
                }

                Label::EvIf => {
                    self.save(self.reg.exp)?;
                    self.save(self.reg.env)?;
                    self.save(self.reg.cont)?;
                    self.reg.cont = Value::Label(Label::EvIfDecide);
                    self.reg.exp = self.cadr(self.reg.exp)?;
                    Label::EvalDispatch
                }
                Label::EvIfDecide => {
                    self.reg.cont = self.restore();
                    self.reg.env = self.restore();
                    self.reg.exp = self.restore();
                    if self.reg.val.is_true() {
                        self.reg.exp = self.caddr(self.reg.exp)?;
                        Label::EvalDispatch
                    } else if !self.cdddr(self.reg.exp)?.is_null() {
                        self.reg.exp = self.car(self.cdddr(self.reg.exp)?)?;
                        Label::EvalDispatch
                    } else {
                        self.reg.val = Value::Unspecified;
                        self.go_to_continue()?
                    }
                }

                // Clauses are tested one by one as in exercise 5.24.
                Label::EvCond => {
                    self.save(self.reg.cont)?;
                    self.reg.unev = self.cdr(self.reg.exp)?;
                    Label::EvCondLoop
                }
                Label::EvCondLoop => {
                    if self.reg.unev.is_null() {
                        self.reg.val = Value::Unspecified;
                        self.reg.cont = self.restore();
                        self.go_to_continue()?
                    } else {
                        self.reg.exp = self.car(self.reg.unev)?;
                        if self.car(self.reg.exp)? == keywords.else_ {
                            self.reg.unev = self.cdr(self.reg.exp)?;
                            Label::EvSequence
                        } else {
                            self.save(self.reg.unev)?;
                            self.save(self.reg.env)?;
                            self.reg.exp = self.car(self.reg.exp)?;
                            self.reg.cont = Value::Label(Label::EvCondDecide);
                            Label::EvalDispatch
                        }
                    }
                }
                Label::EvCondDecide => {
                    self.reg.env = self.restore();
                    self.reg.unev = self.restore();
                    if self.reg.val.is_false() {
                        self.reg.unev = self.cdr(self.reg.unev)?;
                        Label::EvCondLoop
                    } else {
                        self.reg.unev = self.cdar(self.reg.unev)?;
                        if self.reg.unev.is_pair() {
                            Label::EvSequence
                        } else {
                            // A clause without actions has the value of its test.
                            self.reg.cont = self.restore();
                            self.go_to_continue()?
                        }
                    }
                }

                Label::EvAssignment => {
                    self.reg.unev = self.cadr(self.reg.exp)?;
                    self.save(self.reg.unev)?;
                    self.reg.exp = self.caddr(self.reg.exp)?;
                    self.save(self.reg.env)?;
                    self.save(self.reg.cont)?;
                    self.reg.cont = Value::Label(Label::EvAssignment1);
                    Label::EvalDispatch
                }
                Label::EvAssignment1 => {
                    self.reg.cont = self.restore();
                    self.reg.env = self.restore();
                    self.reg.unev = self.restore();
                    self.set_variable_value(self.reg.unev, self.reg.val, self.reg.env)?;
                    self.reg.val = keywords.ok;
                    self.go_to_continue()?
                }

                Label::EvDefinition => {
                    self.reg.unev = self.definition_variable(self.reg.exp)?;
                    self.save(self.reg.unev)?;
                    self.reg.exp = self.definition_value(self.reg.exp)?;
                    self.save(self.reg.env)?;
                    self.save(self.reg.cont)?;
                    self.reg.cont = Value::Label(Label::EvDefinition1);
                    Label::EvalDispatch
                }
                Label::EvDefinition1 => {
                    self.reg.cont = self.restore();
                    self.reg.env = self.restore();
                    self.reg.unev = self.restore();
                    self.define_variable(self.reg.unev, self.reg.val, self.reg.env)?;
                    self.reg.val = keywords.ok;
                    self.go_to_continue()?
                }
            };
        }
    }
}
