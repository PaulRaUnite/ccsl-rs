# CCSL-RS
This a library implementing various structures and algorithms related to CCSL and its format LightCCSL.

These include:
- types for constraints
- a clSTS representation and CCSL-to-clSTS translation
- specification generation
- LightCCSL parsing and rendering

Tools:
- `lccsl-gen` specification generator. More at [](#lightccsl-specification-generator).
- `lccsl2dot` translates LightCCSL specifications into a graphviz DOT graph
- `lccsl-sort` uses

Experiments:
- sorting and approximation algorithms, their testing, evaluation and plotting results
- abstract interpretation model checker (nonoperational)

## Rust installation
Please go to https://www.rust-lang.org/tools/install and follow the instructions 
for your platform.

One can test if everything installed correctly by opening new terminal window and calling `cargo`.

Be sure Rust env variables were added to `~/.profile`.
Session restart (log out/log in;system restart) may be required.

## Tools' usage
### Without cloning:
```bash
cargo install --git=https://github.com/PaulRaUnite/ccsl-rs tools
```
This will add `lccsl2dot`, `lccsl-gen`, `lccsl-sort`, `pn2lccsl` command-line tools.
Adding `--bin <tool-name>` will only install that tool.

### With cloning:
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

## Experiments
### Optimization and approximation of CCSL solving
How to reproduce:
1. run `cargo run --release --bin main -- generate ./<path to data>/`,
2. run `cargo run --release --bin main -- analyze ./<path to data>/<dataset>/`
3. inspect generated CSV files in `./<path to data>/<dataset>/csv/`

## LightCCSL specification generator
Generates several families of LightCCSL specifications:
- random connected specification (if constrains would be hyperedges and clocks vertices, the resulting digraph is [weakly connected](https://en.wikipedia.org/wiki/Connectivity_(graph_theory)))
- processing network: a digraph from a set of inputs into a set of outputs through several layers of vertices, this digraph's vertices are treated as clocks and edges as precedence constraints. Can add a random backpressure constraint between inputs and outputs.
- precedence trees with or without backpressure constraints, it is a special case of processing network and are not randomly generated; produces all not isomorphic trees/specifications of a given size. 
- cycles with "heads" and "tails": chains of precedences that are bounded in between the tail and head, can add another backpressure constraint between start and end clocks.

### Typical usage
`lccsl-gen one --seed=0 --size=5` which writes a random specification into stdout. 

To obtain a random seed (at least in Linux (Unix?)), one can utilize `echo $RANDOM` or directly as an argument
`lccsl-gen dir rand ./specs/ --amount=3 --seed=$RANDOM --size=5`.

The seed then can be found in the name of the generated directory, if flag `--flatten` is not used.

### Examples
1. generate a directory of random specifications
    ```bash
   lccsl-gen dir rand ./specs/ --amount=3 --seed=12345 --size=5
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
   lccsl-gen one --seed=12345 --size=5
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

All the options are documented and can be retrieved by calling `lccsl-gen --help` and `lccsl-gen <subcommand> --help`.
