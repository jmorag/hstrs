# hstrs
*hstrs* is a haskell rewrite of [*strs*](https://github.com/rrybarczyk/strs.git), which is a modern alternative to [*strings*](https://linux.die.net/man/1/strings).

```
hstrs 0.0.1

Usage: hstrs ([-n|--number <number>] [-o|--offset <offset>] FILES |
             (-v|--version))
  A modern alternative for strings, in haskell

Available options:
  -n,--number <number>     Specify the minimum string length, where the number
                           argument is a positive decimal integer. (default: 4)
  -o,--offset <offset>     Write each string preceded by its byte offset from
                           the start of the file. The format shall be dependent
                           on the single character used as the format
                           option-argument:
                               d         The offset shall be written in decimal.
                               o         The offset shall be written in octal.
                               x         The offset shall be written in hexadecimal.
  -v,--version             Print the version
  -h,--help                Show this help text
```
