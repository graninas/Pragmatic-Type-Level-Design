#include <iostream>
#include <vector>
#include <memory>
#include <typeinfo>

// Existential Fight Club in C++, OOP variant

// Demonstration of the OOP wrapping approach that is not existential.

// Fight club rule interface
class FightClubRule
{
public:
  virtual ~FightClubRule() = default;
  virtual std::string explain() const = 0;
};

// Rules
class FirstRule : public FightClubRule
{
public:
  std::string explain() const override
  {
    return "You do not talk about Fight Club.";
  }
};

class SecondRule : public FightClubRule
{
public:
  std::string explain() const override
  {
    return "You DO NOT talk about Fight Club.";
  }
};

class ThirdRule : public FightClubRule
{
public:
  std::string explain() const override
  {
    return "If someone says stop, goes limp, or taps out, the fight is over.";
  }
};

// Container that uses dynamic polymorphism
class Secrecy
{
  std::shared_ptr<FightClubRule> rule;

public:
  template <typename T>
  Secrecy(std::shared_ptr<T> r) : rule(std::move(r)) {}

  std::string explain() const
  {
    return rule->explain();
  }
};

// Program
int main()
{
  std::vector<Secrecy> rules = {
      Secrecy(std::make_shared<FirstRule>()),
      Secrecy(std::make_shared<SecondRule>()),
      Secrecy(std::make_shared<ThirdRule>())};

  for (const auto &rule : rules)
  {
    std::cout << rule.explain() << std::endl;
  }

  std::vector<std::shared_ptr<FightClubRule>> rules_direct = {
    std::make_shared<FirstRule>(),
    std::make_shared<SecondRule>(),
    std::make_shared<ThirdRule>()
  };

  for (const auto &rule : rules_direct)
  {
    std::cout << rule->explain() << std::endl;
  }

  return 0;
}
