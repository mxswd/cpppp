# CPPPP

**Experimental** - **Proof of concept**

## Description

Cpppp introduces "Quoted Initialisers" to C. Quoted initialisers are C initialisers that are executed before C compilation and the results substituted (similar to macros). For example:

```c
int x = 4;
iny y = 2;
int z = @|c_hs|"(+) %d %d"|x, y|;
```

Here we use the `c_hs` transformer to call Haskell's `(+)` function to add two C integers. Cpppp expands this initialiser out into a C foreign function interface call and a Haskell foreign function export.

This is not limited to FFI code generation, you can add your own code transformers! Simply implement the following data type:

```haskell
data Transform = forall a . Transform
	{ _init :: IO a
	, _trns :: a -> String -> [Id] -> SrcLoc -> IO (a, Initializer)
	, _fin  :: a -> IO ()
	}
```

Using this, we can type check and code generate SQL expressions, regex expressions, Auto Layout strings, etc. Anything you might of used quasi-quotation for in Haskell.

## Status

`language-c-quote` can not ingest macros yet, so it will not work for anything serious yet.

## Dependencies

The following repos and branches are required to be `cabal install`-ed:

- `git@github.com:maxpow4h/language-c-quote.git` branch: `quoted-init`
