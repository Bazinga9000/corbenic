# Corbenic Roadmap

This docuemnt lists things explicitly deferred or planned for later versions of the langauge:

## Lexer/Parser Concerns

- [ ] Higher order operators.
- [ ] Replace/Tweak `makeExprParser` to more explicitly control same-precedence unary associator rules

## Expander Concerns

- [ ] Expander pass to allow for macro level operators
  - [ ] `𓂀` for defining and getting linear lenses on general invertible functions 
  - [ ] User-definable macros
  - [ ] Builtin to produce eliminators of types
- [ ] Automatic derivation of all transitive `Convertible` instances in a way that doesn't make inference impossible

## Typechecker Concerns

- [ ] Generalized Algebraic Data Types (this one will take a long long time!)
