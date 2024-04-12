# Turing Machine demo application

The application is an implementation of the Turing Machine
that has the following features:

- Simple command-line interface
- Type Class or Free Monad interfacing showcase selection
- Loading predefined tapes
- Loading tapes from files and command line
- Loading predefined rules
- Loading rules from config files
- Running a rule on tape
- Listing rules and tapes

### The code demonstrates the following concepts:

- The application architecture in the presence of type-level subsystem
- Separation of eDSLs, interfaces and implementation
- Type class interfacing showcase
- Free monad interfacing showcase
- Type-level and value-level rule eDSLs
- Granular TypeSelector design pattern used to empower
  the rule langauge to have two representations:
  static type-level and dynamic value-level with the ADT fields
  chosen accordingly to the level
- Static materialization of type-level rules into value-level ones
- Storate for static type-level and dynamic value-level rules
  that uses the Existentification design pattern
- Evaluators for both static and dynamic rules
- Typed configs for externally defined rules
- Testing possibilities for rules


### Build with stack and run:

*Running with type class interfacing (default):*

> stack exec turing-machine

*Running with free monad interfacing:*

> stack exec turing-machine "free-monad"
