# Pancake Verifier

## Dependencies

 - JDK11 or higher
 - Viper toolchain
 - CakeML compiler

`VIPER_HOME` needs to point to the the `backends` directory from the Viper toolchain installation. 
If installed via the VS Code extension this can usually be found under `~/.config/Code/User/globalStorage/viper-admin.viper/Stable/ViperTools/backends`.

`libjvm.so` is needed for the JNI between the Viper Server and the transpiler to work.

The `cakeML` compiler needs to either be in the `PATH` or pointed to by the `CAKE_ML` environment variable.

## How to run

### Standalone

Run in standalone mode, which prints the generated Viper code to stdout, using `cargo r --bin pancake2viper -- example.pnk`.

### VS Code

The VS Code extension can be installed from [here](https://marketplace.visualstudio.com/items?itemName=alegnani.pancake-ide). 
Note that the extension lacks behind the development branches quite significantly.
