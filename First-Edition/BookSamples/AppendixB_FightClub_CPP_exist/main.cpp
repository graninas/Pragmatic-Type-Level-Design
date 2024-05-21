#include <iostream>
#include <vector>
#include <functional>

// Existential Fight Club in C++

// Demonstration of the existential wrapping approach.


// Type class interface
template<typename T>
concept FightClubRule = requires(T rule) {
  {
    rule.explain()
  } -> std::same_as<std::string>;
};

// Rules
struct FirstRule
{
  std::string explain()
  {
    return "You do not talk about Fight Club.";
  }
};

struct SecondRule
{
  std::string explain()
  {
    return "You DO NOT talk about Fight Club.";
  }
};

struct ThirdRule
{
  std::string explain()
  {
    return "If someone says stop, goes limp, or taps out, the fight is over.";
  }
};


// Existential wrapper with lambdas
struct Secrecy
{
  std::function<std::string()> _ruleF;

  Secrecy() = default;
  Secrecy(FightClubRule auto&& rule)
  {
    _ruleF = [&]()
    {
      return rule.explain();
    };
  }

  std::string explain()
  {
    return _ruleF();
  }
};

// Program
int main()
{
  auto rule1 = FirstRule{};
  auto rule2 = SecondRule{};
  auto rule3 = ThirdRule{};

  std::vector<Secrecy> rules = {
    Secrecy(rule1),
    Secrecy(rule2),
    Secrecy(rule3)
  };

  for (auto &&rule : rules)
    std::cout << rule.explain() << std::endl;

  return 0;
}
