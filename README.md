[![License workflow](https://img.shields.io/badge/License-GPLv3-yellow.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html "View GPLv3 license")
[![CMAKE](https://img.shields.io/badge/cmake-3.27.7-red)](https://github.com/Kitware/CMake/releases/tag/v3.27.7 "View cmake")
[![GCC compatibility](https://img.shields.io/badge/gcc-14.1.0-green)](https://gcc.gnu.org/gcc-14/ "View GCC")


# HeatFlow

by Harry Mclean, Francis Huw Davies, Ned Thaddeus Taylor, and Steven Paul Hepplestone

HeatFlow is Fortran-based a software package for modelling dynamical heat transport in systems using finite difference methods.
The software is primarily designed to utilise the Cattaneo method.
However, the Fourier method can be used instead.

**IMPORTANT NOTICE: Repository Migration to GitHub**

Dear users and contributors,

This repository has been to be migrated from the University of Exeter GitLab to GitHub to facilitate community interaction and support.
The latest version, updates, and collaboration now take place on this GitHub repository.

**GitLab Repository (Archived):** https://git.exeter.ac.uk/hepplestone/heatflow-mk2

## Why the Migration?

It was decided that this project should be migrated to allow for better community support (i.e. allowing community users to raise issues).
All information has been ported over where possible.
Releases prior to `HeatFlow_CattaneoPaper` have had their history modified to remove history of files over 50MB in size.

## How to Contribute on GitHub?

Thank you for your understanding and continued support!

---


## VS Code and GitHub Copilot Setup

If you are using [Visual Studio Code](https://code.visualstudio.com/), this repository includes recommended extensions for an improved development experience.
When you open the repository in VS Code you will be prompted to install them, or you can install them manually via the Extensions panel (`Ctrl+Shift+X` / `Cmd+Shift+X`).

To connect your **GitHub Copilot Education** account to VS Code:

1. Install the [GitHub Copilot](https://marketplace.visualstudio.com/items?itemName=GitHub.copilot) and [GitHub Copilot Chat](https://marketplace.visualstudio.com/items?itemName=GitHub.copilot-chat) extensions (recommended automatically by this repo).
2. Open the VS Code Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`) and run **"GitHub Copilot: Sign In"**, or click the Copilot icon in the status bar.
3. Follow the browser prompt to authorise VS Code with your GitHub account. Make sure you sign in with the GitHub account that has the Copilot Education benefit activated (verify at [github.com/settings/copilot](https://github.com/settings/copilot)).
4. Once authorisation is complete, Copilot suggestions will be active automatically in the editor.

> **Note:** GitHub Copilot Education is available free of charge to verified students and educators through the [GitHub Education](https://education.github.com/) programme.

---

## Requirements

- Fortran compiler supporting Fortran 2003 standard or later
- fpm or CMake

The software bas been developed and tested using the following Fortran compilers:
- gfortran -- gcc 13.2.0
- gfortran -- gcc 14.1.0

## Installation

To install HeatFlow, the source must be obtained from the git repository. Use the following commands to get started:
```
 git clone https://github.com/ExeQuantCode/HeatFlow.git
 cd HeatFlow
```

### fpm

To install using fpm, run the following command in the repository root directory:

```
fpm build --profile=release
```

To execute the code, use

```
fpm run HeatFlow --profile release -- [ALL PROGRAM OPTIONS]
```

### cmake

For cmake installation, start within the repository root directory, run the following commands:

```
mkdir build
cd build
cmake [-DCMAKE_BUILD_TYPE=Release] ..
make install
```

This will build and install the executable in the following directory:
```
${HOME}/.local/HeatFlow/bin/HeatFlow
```

This executable can now be called to run the HeatFlow software package and simulate heat transport.
If the `${HOME}/.local/HeatFlow/bin` is added to your `PATH` environment variable, then the program can be called as a terminal command.
This can be done with the following command (works on a per-terminal basis, if you want to update it for all, include this in your source shell file):

```
export PATH="${PATH}:${HOME}/.local/HeatFlow/bin"
```

To execute the program, use the following command:

```
HeatFlow
```
