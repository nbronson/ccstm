# Contexts, Modes and Binding

The most common way to access `Ref`s is inside an atomic block that introduces
an implicit `Txn` into the current lexical scope.  When atomicity is only
required for the duration of a single `Ref` operation, however, `Ref.single`
results in code that is both more concise and faster.  The following two pieces
of code are equivalent:

    def incr(x: Ref[BigInt]) {
        STM.atomic { implicit t => x.transform(_ + 1) }
    }
    def incr(x: Ref[BigInt]) {
        x.single.transform(_ + 1)
    }

Method calls on a `Ref.Bound` instance returned from `Ref.single` act
as if a new transaction is created for committed for their execution.
These 'single method transactions' will nest into an existing transaction
if one is active, otherwise they will use an optimized code path to run
atomically outside a transaction.  The `context` method of the bound
reference returned from `Ref.single` will always return the object
`Single`, regardless of the current transaction context.  The lookup to
determine the context in which the single-operation transaction will be
nested will be performed during *each* invocation of the method.  This
means that all of the instances returned from `x.single` are equivalent,
and they can be used for the lifetime of the program if desired.

