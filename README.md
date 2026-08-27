# shake-fpga

Shake-based build system for Haskell-based FPGA projects, featuring:

1. [clash](https://clash-lang.org/) as HDL
2. [verilator](https://www.veripool.org/verilator/) for simulation and testing
3. [vivado](https://www.xilinx.com/products/design-tools/vivado.html) for
   synthesis and bitstream generation

## Assumptions/Dependencies

The following cli tools should be in PATH:

- `vivado`: if you want to build bitstreams
- `verilator`, `pkg-config` and c/c++ compilers: if you want to build the
  verilated models

The program should be run from the project root.

A `shake-fpga.yaml` file should be present in the project root.
