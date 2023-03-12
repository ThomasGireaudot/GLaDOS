# Glados

## Introduction

Last project and end of third year project of Epitech of the module "B5 - Advanced Functional Programming /
B-FUN-500 / Haskell"

The goal of this project is to create a language and its interpreter in Haskell, initially relying on the Lisp language.

You can find the subject [here](./res/B-FUN-500_GLaDOS.pdf).

## Getting Started

### Installation

> Make sure to have [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) and [make] (https://www.gnu.org/software/make/) installed

```
git clone git@github.com:EpitechPromo2025/B-FUN-500-LIL-5-2-glados-hugo.delille.git
```

### Usage
```
make
```

```
./glados
```

## Syntax
To define a variable:
```
x = 5
y = #f
```

To define a function:
```
define functionName (parameters) (function body)
```

```
define add (a b) (+ a b)
define equal (a b) (if (a == b) 1 2)
```

Condition :
```
if (condition) (then) (else)
if (a == 6) 1 2
if (b == y) (+ 1 b) (+ 1 y)
```

## Operators

'+' addition

'-' soustraction

'*' multiplication

'/' division

'%' modulo

'=' equal

'<' less than

'>' more than

```
5 + 6
4 / 7
9 % 5
```

## Launch unit testing
```
stack test
```


## Contributors

| [<img src="https://github.com/gabinheylen.png?size=85" width=85><br><sub>Gabin Heylen</sub>](https://github.com/gabinheylen) | [<img src="https://github.com/ThomasGireaudot.png?size=85" width=85><br><sub>Thomas Gireaudot</sub>](https://github.com/ThomasGireaudot)
| :---: | :---: | :---: | :---: | :---: | :---: |
