# CCSL-RS

## Rust installation
Please go to https://www.rust-lang.org/tools/install and follow the instructions 
for your platform.

One can test if everything installed correctly by opening new terminal window and calling `cargo`.

Be sure Rust env variables were added to `~/.profile`.
Session restart (log out/log in;system restart) may be required.

## Optimization and approximation of CCSL solving
How to reproduce:
1. run `cargo run --release --bin main -- generate ./<path to data>/`,
2. run `cargo run --release --bin main -- analyze ./<path to data>/<dataset>/`
3. inspect generated CSV files in `./<path to data>/<dataset>/csv/`

## LightCCSL specification generator

Prerequisites: Rust and Git installed, few Gb of free disk space (for dependencies and module object files).

Getting started:
```bash
git clone https://github.com/PaulRaUnite/ccsl-rs.git
cargo run --release --bin lccsl-gen -- --help
```

or 

```bash
git clone https://github.com/PaulRaUnite/ccsl-rs.git
cargo build --release --bin lccsl-gen
./target/release/lccsl-gen --help
# or copy lccsl-gen(.exe?) to anywhere you like and run
```

### Typical usage
> `cargo run --release --bin lccsl-gen -- one --seed=0 --size=5`

which writes a specification into the stdout. 

To obtain a random seed (at least in Linux (Unix?)), one can utilize `echo $RANDOM` or directly as an argument
> `cargo run --release --bin lccsl-gen -- dir ./specs/ --amount=3 --seed=$RANDOM --size=5`.

The seed then can be found in the name of the generated directory, if flag `--flatten` is not used.

The tool uses parallelization of specification generation and writing into files. Can be disabled with `no_par` option.

#### Examples
1. generate a directory of random specifications
    ```bash
   cargo run --release --bin lccsl-gen -- dir ./specs/ --amount=3 --seed=12345 --size=5
    ```
    result:
   ```
   specs/
   └── 12345
       ├── 5-10082841213727481447.lc
       ├── 5-16290607951801111627.lc
       └── 5-5643454701289460054.lc
    ```
   where `5-10082841213727481447.lc`
   ```text
   Specification spec_5_10082841213727481447 {
    Clock c0,c1,c2,c3,c4,c5,c6,c8
    [
        Let c6 be c0+c8
        Precedence c0 < c4
        Let c6 be c1+c4
        Let c4 be c3*c5
        Exclusion c2 # c8
    ]
   }
   ```
2. generate one specification (can be used to rebuild specification if only its name is available, i.e seed and size)
   ```bash
   cargo run --release --bin lccsl-gen -- one --seed=12345 --size=5
   ```
   stdout:
   ```text
   Specification spec_5_12345 {
    Clock c0,c1,c3,c5,c6
    [
        Exclusion c0 # c3
        Let c0 be c1*c6
        Precedence c6 <= c1
        Precedence c6 <= c1
        Let c3 be c1+c5
    ]
   }
   ```
   With seed `10082841213727481447` same output as in the file `5-10082841213727481447.lc`.

All the options are documented and can be retrieved
by calling `cargo run --release --bin lccsl-gen -- --help` or `cargo run --release --bin lccsl-gen -- <subcommand> --help`.

### General algorithm
`dir` subcommand:
1. Initialize a random generator with the provided seed
2. Generate a list of seeds by the generator, of size `amount`
3. Iteratively, using these seeds and provided size, generate and write specifications

`one` subcommand (basically **the** algorithm):
1. Initialize a random generator with seed
2. While number of constrains is not `size`:
   1. Randomly choose constraint type
   2. Randomly choose clocks for constrain arguments from allowed range (`2*size` for now)
   3. Make sure at least one clock was selected in previous constraints
      (for "connectivity" between constrains)