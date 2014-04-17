Haskell style guide
===================

This document describes the preferred coding style for
this project. It tries to cover the major areas of formatting and
naming. When something is not covered by this guide, you should stay
consistent with the code in other modules.

Table of contents
-----------------
1. [Formatting](#1formatting)
  1. [Line length](#1iline-length)
  2. [Indentation](#1iiindentation)
  3. [Blank lines](#1iiiblank-lines)
  4. [Whitespace](#1ivwhitespace)
  5. [Data declarations](#1vdata-declarations)
  6. [List declarations](#1vilist-declarations)
  7. [Pragmas](#1viipragmas)
  8. [Hanging lambdas](#1viiihanging-lambdas)
  9. [Export lists](#1ixexport-lists)
  10. [If-then-else clauses](#1xif-then-else-clauses)
  11. [Case expressions](#1xicase-expressions)
2. [Imports](#2imports)
3. [Comments](#3comments)
  1. [Punctuation](#3ipunctuation)
  2. [Top-level definitions](#3iitop-level-definitions)
4. [Naming](#4naming)
  1. [Modules](#4imodules)
5. [Dealing with laziness](#5dealing-with-laziness)
  1. [Data types](#5idata-types)
  2. [Functions](#5iifunctions)
6. [Miscellaneous](#6miscellaneous)
  1. [Point-free style](#6ipoint-free-style)
  2. [Warnings](#6iiwarnings)

1.&emsp;Formatting
-------------

### 1.i.&emsp;Line length

Lines should not be longer than *80 characters*.

### 1.ii.&emsp;Indentation

Tabs must not be used; use spaces for indenting. Indent code blocks
with *4 spaces*.  Indent the `where` keyword two spaces to set it
apart from the rest of the code, and indent the definitions in a
`where` clause 2 spaces. Some examples:

```haskell
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

### 1.iii.&emsp;Blank lines

Use one blank line between top-level definitions. Do not place blank lines between
type signatures and function definitions.  Add one blank line between
functions in a type class instance declaration if the functions bodies
are large.

### 1.iv.&emsp;Whitespace

Surround binary operators with a single space on either side; this is
optional for arithmetic operators. Do not insert a space after the
lambda symbol (`\`).

### 1.v.&emsp;Data declarations

Align constructors in a data type definition. For example:

```haskell
data Tree a = Branch !a !(Tree a) !(Tree a)
            | Leaf
```

For long type names, the following formatting is also acceptable:

```haskell
data HttpException
    = InvalidStatusCode Int
    | MissingContentHeader
```

Format records as follows:

```haskell
data Person = Person
    { firstName :: !String  -- ^ First name
    , lastName  :: !String  -- ^ Last name
    , age       :: !Int     -- ^ Age
    } deriving (Eq, Show)
```

### 1.vi.&emsp;List declarations

Align the elements in the list. For example:

```haskell
exceptions =
    [ InvalidStatusCode
    , MissingContentHeader
    , InternalServerError
    ]
```

You may omit the first newline.

```haskell
directions = [ North
             , East
             , South
             , West
             ]
```

### 1.vii.&emsp;Pragmas

Place pragmas immediately following the function they apply to.
For example:

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

In the case of data type definitions, you must put the pragma before
the type it applies to. For example:

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### 1.viii.&emsp;Hanging lambdas

You may indent the code following a "hanging" lambda. Some examples:

```haskell
bar :: IO ()
bar = forM_ [1, 2, 3] $ \n -> do
          putStrLn "Here comes a number!"
          print n

foo :: IO ()
foo = alloca 10 $ \a ->
      alloca 20 $ \b ->
      cFunction a b
```

### 1.ix.&emsp;Export lists

Format export lists as follows:

```haskell
module Data.Set
    (
      -- * The @Set@ type
      Set
    , empty
    , singleton

      -- * Querying
    , member
    ) where
```

### 1.x.&emsp;If-then-else clauses

Guards and pattern matches should be preferred over if-then-else
clauses where possible.  Short cases should be placed on a single line
when line length allows it.

When writing non-monadic code (i.e., when not using `do`) and guards
and pattern matches cannot be used, you can align if-then-else clauses
you like you would normal expressions:

```haskell
foo = if ...
      then ...
      else ...
```

Otherwise, you should be consistent with the 4-space indent rule, and the
`then` and the `else` keyword should be aligned.  Examples:

```haskell
foo = do
    someCode
    if condition
        then someMoreCode
        else someAlternativeCode
```

```haskell
foo = bar $ \qux -> if predicate qux
    then doSomethingSilly
    else someOtherCode
```

The same rule applies to nested do blocks:

```haskell
foo = do
    instruction <- decodeInstruction
    skip <- load Memory.skip
    if skip == 0x0000
        then do
            execute instruction
            addCycles $ instructionCycles instruction
        else do
            store Memory.skip 0x0000
            addCycles 1
```

### 1.xi.&emsp;Case expressions

The alternatives in a case expression can be indented using either of
the two following styles:

```haskell
foobar = case something of
    Just j  -> foo
    Nothing -> bar
```

or as

```haskell
foobar = case something of
             Just j  -> foo
             Nothing -> bar
```

Align the `->` arrows when it helps readability.

2.&emsp;Imports
----------

Imports should be grouped in the following order:

1. standard library imports
2. related third party imports
3. local application/library specific imports

Put a blank line between each group of imports.  The imports in each
group should be sorted alphabetically by module name.

Always use explicit import lists or `qualified` imports for standard
and third-party libraries.  This makes the code more robust against
changes in these libraries.  Exception: the Prelude.

3.&emsp;Comments
-----------

### 3.i.&emsp;Punctuation

Write proper sentences; start with a capital letter and use proper
punctuation.

### 3.ii.&emsp;Top-level definitions

Comment every top-level function (particularly exported functions),
and provide a type signature; use Haddock syntax in the comments.
Comment every exported data type.  Function example:

```haskell
-- | Send a message on a socket.  The socket must be in a connected
-- state.  Returns the number of bytes sent.  Applications are
-- responsible for ensuring that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions, the documentation should give enough information to
apply the function without looking at the function's definition.

Record example:

```haskell
-- | Bla bla bla.
data Person = Person
    { age  :: !Int     -- ^ Age
    , name :: !String  -- ^ First name
    }
```

For fields that require longer comments, format them like so:

```haskell
data Record = Record
    { -- | This is a very very very long comment that is split over
      -- multiple lines.
      field1 :: !Text
      
      -- | This is a second very very very long comment that is split
      -- over multiple lines.
    , field2 :: !Int
    }
```

### 3.iii.&emsp;End-of-line comments

Separate end-of-line comments from the code using 2 spaces.  Align
comments for data type definitions.  Some examples:

```haskell
data Parser = Parser
    !Int         -- Current position
    !ByteString  -- Remaining input

foo :: Int -> Int
foo n = salt * 32 + 9
  where
    salt = 453645243  -- Magic hash salt.
```

### 3.iv.&emsp;Links

Use in-line links economically.  You are encouraged to add links for
API names.  It is not necessary to add links for all API names in a
Haddock comment.  You should add a link to an API name if

* a user might actually want to click on it for more information and

* only for the first occurrence of each API name in the comment (do not
  bother repeating a link).

4.&emsp;Naming
---------

Use camel case (e.g., `functionName`) when naming functions and upper
camel case (e.g., `DataType`) when naming data types.

For readability reasons, do not capitalize all letters when using an
abbreviation.  For example, write `HttpServer` instead of
`HTTPServer`.  Exception: two letter abbreviations (e.g., `IO`).

### 4.i.&emsp;Modules

Use singular when naming modules; e.g., use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

5.&emsp;Dealing with laziness
------------------------

By default, use strict data types and lazy functions.

### 5.i.&emsp;Data types

Constructor fields should be strict unless there is an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the amount of time the programmer has to
spend thinking about evaluation order.

```haskell
-- Good
data Point = Point
    { pointX :: !Double  -- ^ X coordinate
    , pointY :: !Double  -- ^ Y coordinate
    }
```

```haskell
-- Bad
data Point = Point
    { pointX :: Double  -- ^ X coordinate
    , pointY :: Double  -- ^ Y coordinate
    }
```

Additionally, unpacking simple fields often improves performance and
reduces memory usage:

```haskell
data Point = Point
    { pointX :: {-# UNPACK #-} !Double  -- ^ X coordinate
    , pointY :: {-# UNPACK #-} !Double  -- ^ Y coordinate
    }
```

As an alternative to the `UNPACK` pragma, you may place

```haskell
{-# OPTIONS_GHC -funbox-strict-fields #-}
```

at the top of the file. Including this flag in the file itself instead
of in the `.cabal` file is preferable as the optimization will be
applied even if someone compiles the file using other means (i.e., the
optimization is attached to the source code it belongs to).

Note that `-funbox-strict-fields` applies to all strict fields, not
just small fields (e.g., `Double` or `Int`). If you are using GHC 7.4 or
later, you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### 5.ii.&emsp;Functions

Make function arguments lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []    = acc
    go acc (x:xs) = go (acc + x) xs
```

6.&emsp;Miscellaneous
----------------

### 6.i.&emsp;Point-free style

Avoid over-using point-free style. For example, this is hard to read:

```haskell
f = (g .) . h
```

### 6.ii.&emsp;Warnings

Code should not produce warnings when compiled with `-Wall`.
