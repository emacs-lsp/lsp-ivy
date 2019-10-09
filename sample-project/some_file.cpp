struct SomeSymbol {
   int with_member;
};

namespace some_namespace {
using some_other_symbol = SomeSymbol;

int some_function(int) { return 0; }
}  // namespace some_namespace
