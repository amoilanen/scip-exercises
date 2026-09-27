//! The explicit-control evaluator of section 5.4. Each arm of `execute` runs
//! the instructions after a label of the book's controller and returns the
//! label to go to next.

use crate::machine::{Machine, Result, Value, Value::*, MEMORY_SIZE};

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Label {
    Done,
    EvalDispatch,
    EvApplication,
    EvApplDidOperator,
    EvApplOperandLoop,
    EvApplAccumulateArg,
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

use self::Label::*;

impl Machine {
    pub fn evaluate(&mut self, exp: Value) -> Result<Value> {
        self.exp = exp;
        self.env = self.global_env;
        self.cont = Value::Label(Done);
        self.execute()?;
        Ok(self.val)
    }

    fn go_to_continue(&self) -> Label {
        match self.cont {
            Value::Label(label) => label,
            _ => unreachable!("continue always holds a label"),
        }
    }

    fn execute(&mut self) -> Result<()> {
        let mut label = EvalDispatch;
        loop {
            label = match label {
                Done => return Ok(()),
                EvalDispatch => {
                    if self.memory_used() >= MEMORY_SIZE {
                        self.collect_garbage()?;
                    }
                    self.eval_dispatch()?
                }

                EvApplication => {
                    self.save(self.cont)?;
                    self.save(self.env)?;
                    self.unev = self.cdr(self.exp)?;
                    self.save(self.unev)?;
                    self.exp = self.car(self.exp)?;
                    self.cont = Value::Label(EvApplDidOperator);
                    EvalDispatch
                }
                EvApplDidOperator => {
                    self.unev = self.restore();
                    self.env = self.restore();
                    self.argl = Nil;
                    self.proc = self.val;
                    if self.unev == Nil {
                        ApplyDispatch
                    } else {
                        self.save(self.proc)?;
                        EvApplOperandLoop
                    }
                }
                EvApplOperandLoop => {
                    self.save(self.argl)?;
                    self.exp = self.car(self.unev)?;
                    if self.cdr(self.unev)? == Nil {
                        self.cont = Value::Label(EvApplAccumLastArg);
                    } else {
                        self.save(self.env)?;
                        self.save(self.unev)?;
                        self.cont = Value::Label(EvApplAccumulateArg);
                    }
                    EvalDispatch
                }
                // argl collects the arguments in reverse order.
                EvApplAccumulateArg => {
                    self.unev = self.restore();
                    self.env = self.restore();
                    self.argl = self.restore();
                    self.argl = self.cons(self.val, self.argl);
                    self.unev = self.cdr(self.unev)?;
                    EvApplOperandLoop
                }
                EvApplAccumLastArg => {
                    self.argl = self.restore();
                    self.argl = self.cons(self.val, self.argl);
                    self.proc = self.restore();
                    ApplyDispatch
                }
                ApplyDispatch => {
                    let mut args = self.to_vec(self.argl)?;
                    args.reverse();
                    match self.proc {
                        Primitive(name) => {
                            self.val = self.apply_primitive_procedure(name, &args)?;
                            self.cont = self.restore();
                            self.go_to_continue()
                        }
                        Procedure(i) => {
                            let (lambda, env) = self.procedure_parts(i);
                            let parameters = self.cxr("cadr", lambda)?;
                            self.env = self.extend_environment(parameters, &args, env)?;
                            self.unev = self.cxr("cddr", lambda)?;
                            EvSequence
                        }
                        proc => return Err(self.error("The object is not applicable:", &[proc])),
                    }
                }

                // The caller has saved continue.
                EvSequence => {
                    self.exp = self.car(self.unev)?;
                    if self.cdr(self.unev)? == Nil {
                        self.cont = self.restore();
                    } else {
                        self.save(self.unev)?;
                        self.save(self.env)?;
                        self.cont = Value::Label(EvSequenceContinue);
                    }
                    EvalDispatch
                }
                EvSequenceContinue => {
                    self.env = self.restore();
                    self.unev = self.restore();
                    self.unev = self.cdr(self.unev)?;
                    EvSequence
                }

                EvIf => {
                    self.save(self.exp)?;
                    self.save(self.env)?;
                    self.save(self.cont)?;
                    self.cont = Value::Label(EvIfDecide);
                    self.exp = self.cxr("cadr", self.exp)?;
                    EvalDispatch
                }
                EvIfDecide => {
                    self.cont = self.restore();
                    self.env = self.restore();
                    self.exp = self.restore();
                    if self.val != Bool(false) {
                        self.exp = self.cxr("caddr", self.exp)?;
                        EvalDispatch
                    } else if self.cxr("cdddr", self.exp)? != Nil {
                        self.exp = self.cxr("cadddr", self.exp)?;
                        EvalDispatch
                    } else {
                        self.val = Unspecified;
                        self.go_to_continue()
                    }
                }

                // Clauses are tested one by one, as in exercise 5.24.
                EvCond => {
                    self.save(self.cont)?;
                    self.unev = self.cdr(self.exp)?;
                    EvCondLoop
                }
                EvCondLoop => {
                    if self.unev == Nil {
                        self.val = Unspecified;
                        self.cont = self.restore();
                        self.go_to_continue()
                    } else if self.cxr("caar", self.unev)? == Symbol("else") {
                        self.unev = self.cxr("cdar", self.unev)?;
                        EvSequence
                    } else {
                        self.save(self.unev)?;
                        self.save(self.env)?;
                        self.exp = self.cxr("caar", self.unev)?;
                        self.cont = Value::Label(EvCondDecide);
                        EvalDispatch
                    }
                }
                EvCondDecide => {
                    self.env = self.restore();
                    self.unev = self.restore();
                    if self.val == Bool(false) {
                        self.unev = self.cdr(self.unev)?;
                        EvCondLoop
                    } else {
                        self.unev = self.cxr("cdar", self.unev)?;
                        if self.unev == Nil {
                            self.cont = self.restore();
                            self.go_to_continue()
                        } else {
                            EvSequence
                        }
                    }
                }

                EvAssignment => {
                    self.unev = self.cxr("cadr", self.exp)?;
                    self.save(self.unev)?;
                    self.exp = self.cxr("caddr", self.exp)?;
                    self.save(self.env)?;
                    self.save(self.cont)?;
                    self.cont = Value::Label(EvAssignment1);
                    EvalDispatch
                }
                EvAssignment1 => {
                    self.cont = self.restore();
                    self.env = self.restore();
                    self.unev = self.restore();
                    self.set_variable_value(self.unev, self.val, self.env)?;
                    self.val = Symbol("ok");
                    self.go_to_continue()
                }

                EvDefinition => {
                    self.unev = self.definition_variable(self.exp)?;
                    self.save(self.unev)?;
                    self.exp = self.definition_value(self.exp)?;
                    self.save(self.env)?;
                    self.save(self.cont)?;
                    self.cont = Value::Label(EvDefinition1);
                    EvalDispatch
                }
                EvDefinition1 => {
                    self.cont = self.restore();
                    self.env = self.restore();
                    self.unev = self.restore();
                    self.define_variable(self.unev, self.val, self.env)?;
                    self.val = Symbol("ok");
                    self.go_to_continue()
                }
            };
        }
    }

    fn eval_dispatch(&mut self) -> Result<Label> {
        let exp = self.exp;
        Ok(match exp {
            Int(_) | Float(_) | Str(_) | Bool(_) => {
                self.val = exp;
                self.go_to_continue()
            }
            Symbol(_) => {
                self.val = self.lookup_variable_value(exp, self.env)?;
                self.go_to_continue()
            }
            Pair(_) => match self.car(exp)? {
                Symbol("quote") => {
                    self.val = self.cxr("cadr", exp)?;
                    self.go_to_continue()
                }
                Symbol("set!") => EvAssignment,
                Symbol("define") => EvDefinition,
                Symbol("if") => EvIf,
                Symbol("lambda") => {
                    self.val = self.make_procedure(exp, self.env);
                    self.go_to_continue()
                }
                Symbol("begin") => {
                    self.unev = self.cdr(exp)?;
                    self.save(self.cont)?;
                    EvSequence
                }
                Symbol("cond") => EvCond,
                Symbol("let") => {
                    self.exp = self.let_to_combination(exp)?;
                    EvApplication
                }
                _ => EvApplication,
            },
            _ => return Err(self.error("Unknown expression type", &[exp])),
        })
    }

    fn make_lambda(&mut self, parameters: Value, body: Value) -> Value {
        let rest = self.cons(parameters, body);
        self.cons(Symbol("lambda"), rest)
    }

    fn definition_variable(&self, exp: Value) -> Result<Value> {
        match self.cxr("cadr", exp)? {
            variable @ Symbol(_) => Ok(variable),
            signature => self.car(signature),
        }
    }

    fn definition_value(&mut self, exp: Value) -> Result<Value> {
        match self.cxr("cadr", exp)? {
            Symbol(_) => self.cxr("caddr", exp),
            signature => {
                let (parameters, body) = (self.cdr(signature)?, self.cxr("cddr", exp)?);
                Ok(self.make_lambda(parameters, body))
            }
        }
    }

    /// (let ((v e) ...) body ...) is ((lambda (v ...) body ...) e ...)
    fn let_to_combination(&mut self, exp: Value) -> Result<Value> {
        let bindings = self.to_vec(self.cxr("cadr", exp)?)?;
        let variables = bindings.iter().map(|&b| self.car(b)).collect::<Result<Vec<_>>>()?;
        let operands = bindings.iter().map(|&b| self.cxr("cadr", b)).collect::<Result<Vec<_>>>()?;
        let parameters = self.list(&variables, Nil);
        let lambda = self.make_lambda(parameters, self.cxr("cddr", exp)?);
        let operands = self.list(&operands, Nil);
        Ok(self.cons(lambda, operands))
    }
}

#[cfg(test)]
mod tests;
