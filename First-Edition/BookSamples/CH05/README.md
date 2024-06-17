Cellular automaton app

Based on CH04/Section2


App features:

* Support of 2-dimension 2-state cellular automata (Life-like)
* Static type-level predefined rules
  - ../Assets/Automata/
* Dynamic rules
* External dynamic rules file format
* Command console

Code features:

* Static type-level rule eDSL
  - ../Language/Algorithm.hs
  - ../Cellular/Language/Automaton.hs
* Integrity verification and testing
  - Integrity verification interface: ../Language/Integrity.hs
  - Integrity checks for rules: ../Implementation/Algorithm.hs
  - Integrity tests: IntegritySpec.hs
* Existentified rules & worlds storage
  - ../App/Existential/Rules.hs
  - ../App/Existential/Worlds.hs
* App with console & app state
