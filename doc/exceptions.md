# Exception Handling in Transactions

## User exceptions => rollback + propagate

An exception thrown from inside to outside an atomic block will cause
the transaction to be rolled back, and then the exception will be
rethrown without retrying the transaction.  This is not the only possible
semantics, but in programs that do not use exceptions for control flow
it minimizes the chances that partial results will be committed.  The
internal Scala exception `scala.runtime.NonLocalReturnException` is
handled specially.

If you want to commit an atomic block prior to propagating an exception,
try something like the following:

    val failure = new Ref[Throwable]
    new Atomic { def body {
      try {
        // ...
      } catch {
        case x => failure := x
      }
    }}.run
    if (failure.get != null) throw failure.get


## Exceptions thrown by registered callbacks

If a transaction lifecycle callback or resource handler throws an
exception before the transaction has decided to commit, the transaction
will be rolled back.  The exception will be rethrown after rollback or
commit is complete; the remaining callbacks and handlers will be invoked.
If more than one exception is caught only one of them will be rethrown.


## Internally thrown exceptions subclass `RollbackError`

Rollback of an in-progress transaction requires a non-local transfer of
control, which is accomplished by throwing `RollbackError`.  This will
occur if a transaction becomes invalid or if it is explicitly retried
by the user.  `edu.stanford.ppl.ccstm.RollbackError` should be allowed
to propagate.  Rollbacks use `Error`s instead of `Exception`s because
it is common (if questionable) to catch all `Exception` instances in
error handling code.
