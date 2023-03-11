# Glados

## Introduction

Last project and end of third year project of Epitech of the module "B5 - Advanced Functional Programming /
B-FUN-500 / Haskell"

The goal of this project is to create a language and its interpreter in Haskell, initially relying on the Lisp language.

You can find the subject [here](./res/B-FUN-500_GLaDOS.pdf).

## Getting Started

### Installation

> Make sure to have [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed

```
git clone git@github.com:EpitechPromo2025/B-FUN-500-LIL-5-2-glados-hugo.delille.git
```

### Usage
```
make re
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

```
5 + 6
4 / 7
9 % 5
```


## Contributors

| [<img src="https://github.com/gabinheylen.png?size=85" width=85><br><sub>Gabin Heylen</sub>](https://github.com/gabinheylen) | [<img src="https://github.com/ThomasGireaudot.png?size=85" width=85><br><sub>Thomas Gireaudot</sub>](https://github.com/ThomasGireaudot) | [<img src="https://github.com/hugodml.png?size=85" width=85><br><sub>Hugo Demelenaere</sub>](https://github.com/hugodml) | [<img src="https://github.com/LunessFr.png?size=85" width=85><br><sub>Hugo Delille</sub>](https://github.com/LunessFr) | [<img src="https://github.com/AI-Genesix.png?size=85" width=85><br><sub>Maxime Launay</sub>](https://github.com/AI-Genesix) | [<img src="https://github.com/Enguer2.png?size=85" width=85><br><sub>Enguerrand Van-De-Velde</sub>](https://github.com/Enguer2)
| :---: | :---: | :---: | :---: | :---: | :---: |
