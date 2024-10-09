Traits and type classes

Simple Life-like automaton app.

- Mixed-level
- Newtype idiom
- Automaton type class (trait)
- Simple extensibility with type classes (traits)

# Properties of traits (ChatGPT)

The concepts of *Rust traits*, *Java interfaces*, and *Haskell type classes* share common ground as they all represent ways to define shared behavior across different types. However, they differ significantly in their mechanics, expressiveness, and how they integrate with their respective languages.

### Similarities:
1. **Abstract behavior**: All three (traits, interfaces, and type classes) allow for the definition of abstract behavior that types can implement. They are a way of defining contracts that types must adhere to.
2. **Polymorphism**: Each mechanism enables polymorphism, allowing different types to be treated uniformly based on the behavior they implement, rather than their concrete type.
3. **Method signatures**: They allow types to implement functions or methods that adhere to the defined contract. These methods can later be called on instances of those types.
4. **Generic programming support**: All three concepts support generic programming. They enable defining functions or types in terms of behaviors (via the interface/trait/type class) rather than concrete types, which can lead to more reusable and abstract code.

### Dissimilarities:

#### Rust Traits:
- **Implementation for multiple types**: Traits can be implemented for any type, including both structs and enums, and can be implemented for both user-defined and foreign types. This is known as *trait implementation for external types* or *extension traits* (though orphan rules apply).
- **Associated types**: Rust traits can define *associated types* in addition to methods, which means that a type implementing the trait can specify concrete types that are tied to that trait.
- **Static dispatch**: Traits in Rust can be used in two ways: with static dispatch via generics or dynamic dispatch through trait objects (using `dyn Trait`), depending on the use case.
- **Ownership and borrowing**: Since Rust has a focus on ownership and borrowing, traits interact with lifetimes and borrowing, which is a unique aspect compared to Java and Haskell.

#### Java Interfaces:
- **Single and multiple inheritance**: Java interfaces traditionally did not allow any method implementations (until Java 8 introduced default methods), making them purely abstract. A class can implement multiple interfaces, allowing for a form of multiple inheritance.
- **Dynamic dispatch only**: Java interfaces rely on dynamic dispatch, meaning that the method resolution is done at runtime. This is in contrast to Rust, where you can opt for static dispatch through generics.
- **No associated types**: Java interfaces do not have the concept of associated types like Rust traits. Instead, you need to rely on generic parameters, which can become verbose and less flexible.
- **Subtype polymorphism**: Java interfaces also imply a type hierarchy in the sense that a type implementing an interface can be treated as a subtype of that interface, which is not the case in Haskell or Rust, where type classes and traits are not tied to subtyping.

#### Haskell Type Classes:
- **Type-driven behavior**: Haskell type classes are more focused on type-driven behavior, where you can define behavior based on the type of data. You can think of type classes as more akin to a *system of logic* for defining behavior based on types rather than interfaces in the Java sense.
- **Higher-kinded types**: Haskell supports higher-kinded types with type classes, which means that type classes can work over types that are not fully applied (e.g., classes that operate over functors, monads, etc.). Rust and Java interfaces do not natively support higher-kinded types.
- **Instance resolution and constraints**: In Haskell, instance resolution is performed at compile time based on the type of the data involved, and you can use *type constraints* to require that a type must implement certain classes. Haskell has more powerful and flexible constraint-solving mechanisms than both Rust and Java.
- **No runtime overhead**: Haskell type class dispatch is resolved at compile time with no runtime cost, similar to Rust’s static dispatch, whereas Java's interface method calls incur a runtime cost due to dynamic dispatch.

### Summary:

- **Rust Traits**: Flexible, support both static and dynamic dispatch, can be implemented for external types, and support associated types. They balance flexibility with safety, especially around memory and lifetimes.
- **Java Interfaces**: Primarily dynamic dispatch, interface inheritance, no associated types, and are tied to the object-oriented paradigm and subtyping polymorphism.
- **Haskell Type Classes**: More abstract, focus on compile-time dispatch and logic around types, support higher-kinded types, and have no direct relation to inheritance or subtyping.
