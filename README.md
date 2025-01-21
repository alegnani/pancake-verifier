# Pancake Verifier

The Pancake Verifier is a verifier for the Pancake systems programming language based on the [CakeML](https://cakeml.orgca) verified compiler.
Under the hood it transpiles Pancake into the Viper intermediate language and uses the Viper toolchain to verify it.

## Getting the verifier

The verifier comes both as a standalone program usable via the CLI called `pancake2viper`, found [here](https://github.com/alegnani/pancake-verifier/releases/), and a VS Code extension available [here](https://marketplace.visualstudio.com/items?itemName=alegnani.pancake-ide).

## Dependencies

- JDK11 or newer
- Viper toolchain (available as part of the [VS Code extension](https://marketplace.visualstudio.com/items?itemName=viper-admin.viper) or as a standalone [program](https://github.com/viperproject/viper-ide/releases/tag/v.24.08-release).
- CakeML compiler rev. [d8b47adc](https://cakeml.org/regression.cgi/job/2697) 
- z3 (included as part of the Viper toolchain) 

By default it expects for `cake` to be on the path or `$CAKE_ML` to be set. To override the path use the `--cake <CAKE_PATH>` flag.  
The path to `viperserver.jar` can either be set via `$VIPER_HOME` or using the `--viper <VIPER_PATH>` flag.  
The path to the `z3` executable can either be set via `$Z3_EXE` or using the `--z3 <Z3_PATH>` flag.
A compatible version of z3 can be found at `ViperTools/z3/bin/z3`.  
The Viper installation provided by the Viper VS Code extension can be found under `~/.config/Code/User/globalStorage/viper-admin.viper/Stable/ViperTools/`.

## How to run

### Standalone

To transpile a Pancake file to Viper: 
```bash
pancake2viper transpile foobar.🥞 foobar.vpr
```

To verify a Pancake file: 
```bash
pancake2viper verify foobar.🥞
```

Also supports input via stdin:
```bash
cat foo.🥞 bar.🥞 | pancake2viper verify -
```

To verify a Pancake program running on a 32-bit architecture and a static heap of 1 KiB:
```bash
pancake2viper verify --word-size 32 --heap-size 1024 foobar.🥞
```

### VS Code Extension

Currently the extension is a bit more limited in functionality being stuck on an old version of `pancake2viper`.
On opening or modifying a Pancake file(.🥞 or .pnk) a Viper file is generated with the same name. This file is kept in sync with the Pancake source.
From the Command Palette (Ctrl+Shift+P) the current file can be verified using the `Pancake Verifier: Verify file` command.

## Pancake annotations

```c
fun sum(1 n) {
    /@ requires 0 <= n @/
    /@ ensures retval == n * (n + 1) / 2 @/

    var i = 0;
    var accu = 0;
    while (i < n) {
        /@ invariant 0 <= i && i <= n @/
        /@ invariant accu == (i - 1) * i / 2 @/

        accu = accu + i;
        i = i + 1;
    }
    return accu + n;
}
```

The annotations possible in Pancake are mostly the same as the ones available in Viper. It is recommended to first check out the (well written) [Viper tutorial](https://viper.ethz.ch/tutorial).

Annotations in Pancake are specified inside special block comments `/@ ... @/`.
Note that they always have to be inside a block (`{...}`) and not outside, as is the case in Viper.
```c
// Viper
method foo_bar() 
    ensures ...
{
    while true 
        invariant ...
    {
        ...
    }
}

// Pancake
fun foo_bar() {
    /@ ensures ... @/
    while (true) {
        /@ invariant ... @/
        ...
    }
    return 0;
}
```
The supported Viper annotations are: `requires` for preconditions, `ensures` for postconditions, `assert`, `refute`, `invariant`, `assume`, `inhale` (assume + gain of access permission),`exhale` (assert + loss of access permission), `fold` and `unfold` to fold/unfold predicates.

Annotations can use the arithmetic operators (`+`, `-`, `*`, `/`, `%`), logical operators (`!`, `&&`, `||`, `==>`, `<==>`) and comparison operators (`==`, `!=`, `<`, `<=`, `>`, `>=`) from Viper.
Annotations use Viper's semantics ,e.g. arithmetic operations are unbounded, but the following Pancake operators are supported:
| | |
|---|---|
| Unsigned comparisons | `<+`, `<=+`, `>+`, `>=+` (Not yet implemented) |
| Bitwise operations | `&`, `\|`, `^` (`!` is the boolean operator; TODO: bitwise NOT) |
| Shift operations | `<<`, `>>`, `>>>` |

Annotations support both an existential and a universal quantifier:
```c
forall i: Int :: { optional trigger(s) } 0 < i && i <= 10 ==> ...
exists i: Int :: 0 < i && i <= 10 ==> ...
```

All Pancake variables and arguments can be used in annotations (given that they are in scope).
The return value of a Pancake function can be accessed as `retval`.
For the result of a Viper function use `result` instead.


Some built-in functions are provided for convenience:
| | |
|---|---|
| `bounded8(x)`, `bounded16(x)`, `bounded32(x)`, `bounded64(x)` | Checks whether or not `x` is guaranteed to fit in a `u8`, `u16`, `u32`, `u64` |
| `bounded(x)` | Checks whether or not `x` is guaranteed to fit into a word |

### Reasoning about the heap

To reason about the heap of a Pancake program the `heap` variable can be used. The heap is represented as a word-indexed array of words.
`heap[@base]` is the first element of the heap.
To specify the access permissions of a region of the heap the Viper function `acc` is used e.g. `acc(heap[0])`.
For convenience the `acc(heap[0..42])` syntax can also be used for a region spanning more than a single word. Note that the upper bound is exclusive.

```c
fun main() {
    /@ requires acc(heap[@base..@base+2], write) @/
    /@ requires heap[@base] == 42 @/

    /@ ensures acc(heap[@base..@base+2], write) @/
    /@ ensures heap[@base] == old(heap)[@base] @/ // unchanged
    /@ ensures heap[@base+1] == heap[@base] @/

    var x = lds 1 @base;
    st @base + @biw, x;
    return 0;
}
```

> [!NOTE]
> The `heap[l..u]` syntax is can only be used inside of an `acc`. `heap[x..y] == heap[z..v] is (unfortunately) not going to work.

### Reasoning about shapes

Pancake compound shapes, like `<1, 2, <3, 4> >`, can be reasoned about in annotations.
This can be done by comparing single elements via the dot syntax (`foo.0 == bar.1.2`) or via structural equality (`foo == bar.1`).
Shapes can be return values or arguments of a Pancake function or the argument of Viper predicates or functions.
When used as an argument in Viper predicates or functions use the shape instead of the type.
```c
/@ predicate shape_pred(shape: {1, 2}) {
    shape.0 > 42 && shape.1 == < 1, 2 >
} @/
```

### Reasoning about shared memory

Reasoning about shared memory is done by providing a separate Viper file here in referred to as a "model".
The model consists of Viper fields that hold state and Viper methods that specify the effect of shared memory operations.
This includes the preconditions that must hold before a shared memory read/write, the postconditions (including the value read from the shared memory location) and how this operation affects the device state.

####

### Other examples

Annotation examples, showcasing all of the features, can be found in the [test folder](https://github.com/alegnani/pancake-verifier/tree/main/pancake2viper/tests).
