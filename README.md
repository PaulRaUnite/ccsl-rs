# CCSL-RS

## Rust installation
Go to https://www.rust-lang.org/tools/install and follow the instructions 
for your platform.

For Linux, it is just an oneliner, and one can test if everything installed correctly by 
opening new terminal window and call `cargo`.

Be sure Rust env variables were added to `~/.profile`.
Session restart (log out/log in;system restart) may be needed.

## LightCCSL generator

Prerequisites: Rust and Git installed, few Gb of free disk space (for dependencies and module object files).

Using the tool is extremely easy:
```bash
git clone https://github.com/PaulRaUnite/ccsl-rs.git
cd ./ccsl-rs/tool
cargo run --release --bin lccsl-gen -- --help
```

or 

```bash
git clone https://github.com/PaulRaUnite/ccsl-rs.git
cd ./ccsl-rs/tool
cargo build --release --bin lccsl-gen
cd ..
cd ./target/release/
./lccsl-gen --help
# or copy lccsl-gen(.exe?) to anywhere you like and run
```

> WARNING: The tool uses parallelization of generation by default, comment (`//`) line 
> with `.par_bridge()` if not desired/causes problems.

### Typical usage
> `./lccsl-gen` can be replaced by `cargo run --release --bin lccsl-gen --`

> To obtain a random seed (at least in Linux (Unix?)), one can utilize `echo $RANDOM` or directly as option
> `./lccsl-gen dir --dir=./specs/ --amount=3 --seed=$RANDOM --size=5`.
> The seed then will be found in the name of the generated directory, if flag `--flatten` is not used.


1. generate a directory of random specifications
    ```bash
   ./lccsl-gen dir --dir=./specs/ --amount=3 --seed=12345 --size=5
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
2. generate one specification (for possible integration purposes)
   ```bash
   ./lccsl-gen one --seed=12345 --size=5
   ```
   output
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
by calling `./lccsl-gen --help` or `./lccsl-gen <subcommand> --help`.

### General algorithm
`dir` subcommand:
1. Initialize a random generator with seed
2. Generate a list of seeds by the generator
3. Iteratively, using these seeds and provided size generate specifications

`one` subcommand (basically **the** algorithm):
1. Initialize a random generator with seed
2. While number of constrains is not `size`:
   1. Randomly choose constraint type
   2. Randomly choose clocks for constrain arguments from allowed range (`2*size` for now)
   3. Make sure at least one clock was selected in previous constraints
      (for "connectivity" between constrains)