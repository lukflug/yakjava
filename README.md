# yakjava
Simple programming language consisting of only booleans.


Example:
```
a=input;
rep:
if (a!=true) print a;
goto rep;
```

Following commands exist:
* Assignment (no declaration necessary or allowed, e.g. `a=true;`).
* Print, outputs boolean as `true` or `false`.
* If-statement, same syntax as java.
* Goto and labels, for jumping.

Arbitrarily complex expressions using booleans and logic operators are possible. `input` is a special variable allowing for input from the user using a `(y/n)` prompt.
