# yahl
YetAnotherHaskell: for the LLVM

# Description
`yahl` is mostly a personal project to explore various language constructs.
Technically, its a haskell written for the LVVM that tries to mimic conventional
lambda calculus notation in its syntax.

# Examples

## Variable definition

Defining a variable in `yahl` looks like one would expect:

```
foo = 'bar'
```

## Function Declaration

Function declaration resembles traditional Haskell syntax:

```
foo x -> 2 * x
```

## Lambda functions

The syntax for lambda function declaration:

```
λ x -> 2x
```

This defines an anonymous function that takes one paramters and returns twice the value.

### Multiple Arguments

Functions are curried by default so the following two expressions are equivalent:

```
λ x -> λ y -> x y
```
```
λ x y -> x y
```

## Calling a function

Function invocation resembles traditional haskell syntax

foo x -> 2 * x

foo 2
