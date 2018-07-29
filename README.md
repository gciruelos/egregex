# egregex

Regular expression debugger and optimizer.
*UNDER DEVELOPMENT! PRs Welcome.*


## How to build
Just clone and use stack to build

```
$ git clone https://github.com/gciruelos/egregex
$ cd egregex
$ stack build
```

## How to use

### Enumerating matches
`egregex` can show examples of strings that match a given regular expression.

```
$ stack exec egregex -- --show-matches=10 "a+b*c+"
ac
aac
acc
abc
aaac
aacc
aabc
accc
abcc
abbc
```

If no parameter is supplied to `--show-matches`
then it will enumerate *all* the strings that match the given regular expression

`egregex` can also show examples of strings that do not match a given regular expression.

```
$ stack exec egregex -- --show-mismatches=12 "a+b*c+"
a
b
c

aa
ab
ba
bb
bc
ca
cb
cc
```

The empty line is the empty string.

### More than one regular expression
`egregex` can also enumerate strings that match or don't match several different
regular expressions.

```
$ stack exec egregex -- --matches="a+b*c+" --matches="a*b+c*" --doesnt-match="a*c" --show-matches=12
abc
aabc
abcc
abbc
aaabc
aabcc
aabbc
abccc
abbcc
abbbc
aaaabc
aaabcc
```

### Optimizing regular expressions
`egregex` allows the user to obtain an optimized (shorter)
version of a given regular expression.

```
$ stack exec egregex -- -O1 "a*a+"
a+
```

It can also to merge several requirements into a single regular expression.

```
$ stack exec egregex -- --matches="a+b*c+" --matches="ab+c*" --doesnt-match="abc" -O1
ab((cc)|(b+c))c*
```

If the user wants a faster result in exchange for a worse resulting regular expression,
they can choose a lower optimization level.

```
$ stack exec egregex -- --matches="a+b*c+" --matches="ab+c*" --doesnt-match="abc" -O0
ab((cc)|(bb*c)))|(ab((cc)|(bb*c))c*(c|)
```






