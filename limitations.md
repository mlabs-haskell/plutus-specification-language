
- No way of checking that the output can always be consumed by another transaction.
  This is quite a hard problem, and will need some more thought, because
  even if you show that some output can be consumed by another transaction,
  you have to show that the outputs of that transaction can *also* be consumed,
  and so on, recursively.
  This is necessary for showing that state machines don't get stuck,
  e.g. by having a datum so large that no transaction would be under the limit.
