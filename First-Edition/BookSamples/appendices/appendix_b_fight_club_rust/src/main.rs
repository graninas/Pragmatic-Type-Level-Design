
// Existential Fight Club in Rust

// Demonstration of the existential wrapping approach.

// Trait (type class) interface
trait FightClubRule {
    fn explain(&self) -> String;
}

// Rules
struct FirstRule;
struct SecondRule;
struct ThirdRule;

impl FightClubRule for FirstRule {
    fn explain(&self) -> String {
        "You do not talk about Fight Club.".to_string()
    }
}

impl FightClubRule for SecondRule {
    fn explain(&self) -> String {
        "You DO NOT talk about Fight Club.".to_string()
    }
}

impl FightClubRule for ThirdRule {
    fn explain(&self) -> String {
        "If someone says stop, goes limp, or taps out, the fight is over.".to_string()
    }
}

// Existential wrapper
struct Secrecy {
    rule: Box<dyn FightClubRule>,
}

impl Secrecy {
    fn new<R: 'static + FightClubRule>(rule: R) -> Self {
        Secrecy {
            rule: Box::new(rule),
        }
    }

    fn explain(&self) -> String {
        self.rule.explain()
    }
}


fn main() {
    // List of rules
    let rules: Vec<Secrecy> = vec![
        Secrecy::new(FirstRule),
        Secrecy::new(SecondRule),
        Secrecy::new(ThirdRule),
    ];

    // Printing
    for rule in rules {
        println!("{}", rule.explain());
    }
}
