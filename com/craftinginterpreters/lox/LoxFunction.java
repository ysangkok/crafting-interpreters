package com.craftinginterpreters.lox;

import java.util.List;

class LoxFunction implements LoxCallable {
  private final List<Token> params;
  private final List<Stmt> body;
  private final String name;
  private final Environment closure;
  static LoxFunction fromStatement(Stmt.Function declaration, Environment closure) {
    return new LoxFunction(closure, declaration.params, declaration.body, declaration.name.lexeme);
  }
  static LoxFunction fromLambda(Expr.Lambda lambda, Environment closure) {
    return new LoxFunction(closure, lambda.params, lambda.body, "anonymous function");
  }
  LoxFunction(Environment closure, List<Token> params, List<Stmt> body, String name) {
    this.closure = closure;
    this.params = params;
    this.name = name;
    this.body = body;
  }
  @Override
  public Object call(Interpreter interpreter,
                     List<Object> arguments) {
    Environment environment = new Environment(closure);
    for (int i = 0; i < params.size(); i++) {
      environment.define(params.get(i).lexeme,
          arguments.get(i));
    }

    try {
      interpreter.executeBlock(body, environment);
    } catch (Return returnValue) {
      return returnValue.value;
    }
    return null;
  }
  @Override
  public int arity() {
    return params.size();
  }
  @Override
  public String toString() {
    return "<fn " + name + ">";
  }
}
