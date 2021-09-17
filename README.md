# haskalc
A small calculator written in Haskell.<br>
This is my preferred "Hello World"-program when learning a new programming language.

## Dependencies
- The GHC compiler (or compatible)
- __System.Console.Readline__ (optional)

## Building
```make```<br>
If __System.Console.Readline__ is not installed,<br>
please build with ```make ENABLE_READLINE=0```

## Installation
```sudo make install```<br>
Changing the installation prefix can be done by setting the ```PREFIX=/path/to/prefix``` make variable.
