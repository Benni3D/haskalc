# haskalc
A small calculator written in Haskell.<br>
This is my preferred "Hello World"-program when learning a new programming language.

## Dependencies
- The GHC compiler (or compatible)
- __System.Console.Readline__ (optional)
- __System.Posix.Signals__ (optional)

## Building
```make [Option=Value...]```<br>
| Build Option | Default Value | Description |
|--------------|---------------|-------------|
| ENABLE_READLINE | 1 | Enable libreadline support (things like CTRL+L and history) |
| ENABLE_POSIX | 0 (experimenal) | Enable handling of POSIX signals (like CTRL+C) |


## Installation
```sudo make install```<br>
Changing the installation prefix can be done by setting the ```PREFIX=/path/to/prefix``` make variable.
