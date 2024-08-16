set -e
cat $@ | cpp -P | cake --pancake > pancake.S
gcc -DEVAL -o pancake.out pancake.S ../cake-x64-64/basis_ffi.c && rm pancake.S