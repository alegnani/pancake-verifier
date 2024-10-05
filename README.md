# Pancake Verifier

The Pancake Verifier is a verifier for the Pancake systems programming language based on the [CakeML](https://cakeml.orgca) verified compiler.

## Getting the verifier

The verifier comes both as a standalone program usable via the CLI called `pancake2viper`, found [here](https://github.com/alegnani/pancake-verifier/releases/), and a VS Code extension available [here](https://marketplace.visualstudio.com/items?itemName=alegnani.pancake-ide).

## Dependencies

### JDK11 or newer

The verifier uses the JNI to communicate with the Viper server and therefore requires `libjvm.so`

### Viper toolchain

The Viper toolchain can easiest be obtained by installing the VS Code extension available [here](https://marketplace.visualstudio.com/items?itemName=viper-admin.viper). For the extension the path to the `ViperTools` directory needs to be specified in the settings of the extension.
For the standalone verifier it can either be passed via the `--viper <VIPER_PATH>` argument or the `$VIPER_HOME` environment variable.

### CakeML compiler

The verifier reuses the parser for Pancake from the CakeML compiler, which can be obtained from [here](https://github.com/CakeML/cakeml/releases).
For the extension the path to the `cake` executable needs to be specified in the settings of the extension.
For the standalone verifier it can iether be passed via the `--cake <CAKE_PATH>` argument or the `$CAKE_ML` environment variable.

## How to run

### Standalone

To transpile a Pancake file to Viper: 
```bash
pancake2viper -o <OUTPUT_PATH> foobar.🥞
```

To verify a Pancake file: 
```bash
pancake2viper --verify foobar.🥞
```

Also supports input via stdin:
```bash
cat foo.🥞 bar.🥞 | pancake2viper --verify
```


To verify using a Pancake program running on a 32-bit architecture and a heap of 1 KiB:
```bash
pancake2viper --verify --word-size 32 --heap-size 1024 foobar.🥞
```

### VS Code Extension

Currently the extension is a bit more limited in functionality.
On opening or modifying a Pancake file(.🥞 or .pnk) a Viper file is generated with the same name. This file is kept in sync with the Pancake source.
From the Command Palette (Ctrl+Shift+P) the current file can be verified using the `Pancake Verifier: Verify file` command.

## Maple: Pancake's annotation language

```pancake
fun sum(1 n) {
    /*@ requires 0 <= n @*/
    /*@ ensures retval == n * (n + 1) / 2 @*/

    var i = 0;
    var accu = 0;
    while (i < n) {
        /*@ invariant 0 <= i && i <= n @*/
        /*@ invariant accu == (i - 1) * i / 2 @*/

        accu = accu + i;
        i = i + 1;
    }
    return accu + n;
}
```

The annotations possible in Pancake are mostly the same as the ones available in Viper. For further information follow the [Viper tutorial](https://viper.ethz.ch/tutorial).
Note that they have to be specified inside the function body (only exception are predicates, which can be specified in the top level).
Annotations in Pancake are specified inside special block comments `/*@ ... @*/`.

There are some special reserved variables in annotations that are always available.
These are the return value of the current function `retval` and the array representing the heap, `heap`.

The supported Viper statements are: `requires` for preconditions, `ensures` for postconditions, `assert`, `refute`, `invariant`, `assume`, `inhale` (assume + gain of access permission),`exhale` (assert + loss of access permission), `fold` and `unfold` to fold/unfold predicates.

Annotations can use the arithmetic operators (`+`, `-`, `*`, `/`, `%`), logical operators (`!`, `&&`, `||`, `==>`, `<==>`) and comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`).

Array accesses (or heap accesses) can be written as `arr[i]` (`heap[i]`). To access the fields of a Pancake shape use the normal dot notation `shape.0.1` for a shape `{2, 1}`. Note that the resulting element needs to be of shape `1`.


Maple has both an existential and a universal quantifier:
```viper
forall i: Int :: { optional trigger(s) } 0 < i && i <= 10 ==> ...
exists i: Int :: 0 < i && i <= 10 ==> ...
```

### Built-in functions

 - `acc(log, perm)` or `acc(loc)` used to specify access permissions of a location loc. `perm` can either be `write`, `read`, `wildcard` or a fractional permission `int/int`.
 `acc(loc)` grants the full-permission a.k.a `write`.

 - `len(arr)` the length of an array

Note that other Pancake methods can't be called from inside an annotation.

### Predicates

Predicates can be defined in top level annotations.
These can then be used in fold/unfold statements.
As part of expressions the `unfolding <predicate>(...) in ...` construct can be used.
```pancake
// predicate granting access to the 42nd word of the heap
/* @ predicate IO() {
    acc(heap[42])
} @*/

fun main() {
    /*@ requires IO() @*/
    /*@ ensures IO() @*/

    // in order to reason about heap[42] we first need the access permission
    // to heap[42] which is folded in the IO predicate
    /*@ requires unfolding IO() in heap[42] == 17 @*/
    /*@ ensures unfolding IO() in heap[42] == 1337 @*/

    /*@ unfold IO() @*/
    /*@ assert heap[42] == 17 @*/
    st @base + 42 * 8, 1337;
    /*@ fold IO() @*/
    return 0;
}

```

### Annotation examples

Annotation examples, showcasing all of the features, can be found in the [test folder](https://github.com/alegnani/pancake-verifier/tree/main/pancake2viper/tests).