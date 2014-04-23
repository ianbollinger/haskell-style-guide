Haskell style guide
===================

Many open-source projects have their own style guide: a set of
conventions, sometimes arbitrary, about how to write code for that
project. It is easier to understand a large project when all its source
code has a consistent style. This document tries to cover the major
areas of formatting and naming. When something is not covered by this
guide, you should stay consistent with a project's existing source code.

Table of contents
-----------------
1. [Formatting](#1formatting)
  1. [Lines should be no longer than 80 characters](#1ilines-should-be-no-longer-than-80-characters)
  2. [Indent with spaces, not tabs](#1iiindent-with-spaces-not-tabs)
  3. [Separate top-level definitions with a blank line](#1iiiseparate-top-level-definitions-with-a-blank-line)
  4. [Whitespace](#1ivwhitespace)
  5. [Lines must not have trailing spaces](#1vlines-must-not-have-trailing-spaces)
  5. [Align data type constructors](#1vialign-data-type-constructors)
  6. [Align elements in a list](#1viialign-elements-in-a-list)
  7. [Place pragmas after function definitions](#1viiiplace-pragmas-after-function-definitions)
  8. [Hanging lambdas](#1ixhanging-lambdas)
  9. [Export lists](#1xexport-lists)
  10. [Prefer guards and pattern matches to if expressions](#1xiprefer-guards-and-pattern-matches-to-if-expressions)
  11. [Case expressions](#1xiicase-expressions)
2. [Imports](#2imports)
  1. [Group imported modules by origin](#2igroup-imported-modules-by-origin)
  2. [Sort imports alphabetically](#2iisort-imports-alphabetically)
3. [Comments](#3comments)
  1. [Comment every exported definition](#3icomment-every-exported-definition)
4. [Names](#4names)
  1. [Use camel case for function names](#4iuse-camel-case-for-function-names)
  2. [Use upper camel case for type names](#4iiuse-upper-camel-case-for-type-names)
  3. [Do not use all capitals for acronyms](#4iii-do-not-use-all-capitals-for-acronyms)
  4. [Use the singular for module names](#4ivuse-the-singular-for-module-names)
5. [Strictness](#5strictness)
  1. [Make data types strict by default](#5imake-data-types-strict-by-default)
  2. [Make function arguments lazy by default](#5iimake-function-arguments-lazy-by-default)
6. [Miscellaneous](#6miscellaneous)
  1. [Avoid over-using point-free style](#6iavoid-over-using-point-free-style)
  2. [Code must be warning-free](#6iicode-must-be-warning-free)
  3. [Do not escape newlines in strings](#6iiido-not-escape-newlines-in-strings)

1.&emsp;Formatting
------------------

### 1.i.&emsp;Lines should be no longer than 80 characters

Lines should not be longer than *80 characters*. URLs and inline
markup within comments are an exception to this rule.

### 1.ii.&emsp;Indent with spaces, not tabs

Tabs must not be used; use spaces for indenting. Indent code blocks
with *4 spaces*. Indent the `where` keyword two spaces to set it
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
filter p (x : xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs
```

### 1.iii.&emsp;Separate top-level definitions with a blank line

Use one blank line between top-level definitions. Do not place blank
lines between type signatures and function definitions. Add one blank
line between functions in a type class instance declaration if the
functions bodies are large.

### 1.iv.&emsp;Whitespace

Surround binary operators with a single space on either side; this is
optional for arithmetic operators. Do not insert a space after the
lambda symbol (`\`).

### 1.v.&emsp;Lines must not have trailing spaces

Remove any whitespace at the end of each line of source code.

### 1.vi.&emsp;Align data type constructors

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

### 1.vii.&emsp;Align elements in a list

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

### 1.viii.&emsp;Place pragmas after function definitions

Place pragmas immediately following the function they apply to.
For example:

```haskell
id :: a -> a
id x = x
{-# INLINE id #-}
```

In the case of data type definitions, you must put the pragma before the
type it applies to. For example:

```haskell
data Array e = Array
    {-# UNPACK #-} !Int
    !ByteArray
```

### 1.ix.&emsp;Hanging lambdas

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

### 1.x.&emsp;Export lists

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

### 1.xi.&emsp;Prefer guards and pattern matches to if expressions

Guards and pattern matches should be preferred over if-then-else
expressions where possible. Short cases should be placed on a single line
when line length allows it.

When writing non-monadic code (that is, when not using `do`), and guards
and pattern matches cannot be used, align if-then-else expressions as
you would other expressions:

```haskell
foo = if ...
      then ...
      else ...
```

Otherwise, be consistent with the four-space indent rule. Align the
`then` and `else` keywords. Some examples:

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

The same rule applies to nested `do` blocks:

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

### 1.xii.&emsp;Case expressions

The alternatives in a case expression can either be indented using four
spaces

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

Align the arrows (`->`) when it helps readability.

2.&emsp;Imports
---------------

### 2.i.&emsp;Group imported modules by origin

Imports should be grouped in the following order:

1. standard library imports
2. related third-party imports
3. local application or library specific imports

Put a blank line between each group of imports.

### 2.ii.&emsp;Sort imports alphabetically

The imports in each import group should be sorted alphabetically by
module name.

### 2.iii.&emsp;Qualify imports or explicitly list imported symbols

Always use explicit import lists or `qualified` imports for standard and
third-party libraries. This makes the code more robust against changes
in these libraries. Exception: the Prelude.

3.&emsp;Comments
----------------

### 3.i.&emsp;Comment every exported definition

Comment every top-level function (particularly exported functions), and
provide a type signature; use Haddock syntax in the comments. Comment
every exported data type. Function example:

```haskell
-- | Send a message on a socket. The socket must be in a connected state.
-- Returns the number of bytes sent. Applications are responsible for ensuring
-- that all data has been sent.
send :: Socket      -- ^ Connected socket
     -> ByteString  -- ^ Data to send
     -> IO Int      -- ^ Bytes sent
```

For functions, the documentation should give enough information to apply
the function without looking at the function's definition.

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
    { -- | This is a very very very long comment that is split over multiple
      -- lines.
      field1 :: !Text
      
      -- | This is a second very very very long comment that is split over
      -- multiple lines.
    , field2 :: !Int
    }
```

### 3.iii.&emsp;End-of-line comments

Place at least two spaces between a comment and any preceding code.
Align comments for data type definitions. Some examples:

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

Use in-line links economically. You are encouraged to add links for API
names. It is not necessary to add links for all API names in a Haddock
comment. Only add a link to an API name when a user might want to click
on it. Only add links for the first occurrence of a name in a comment.

4.&emsp;Names
-------------

### 4.i.&emsp;Use camel case for function names

Use camel case (for example, `functionName`) when naming functions.

### 4.ii.&emsp;Use upper camel case for type names

Use upper camel case (for example, `DataType`) when naming data types.

### 4.iii&emsp;Do not use all capitals for acronyms

For readability reasons, only capitalize the first letter of acronyms.
For example, write `HttpServer` instead of `HTTPServer`. Exception: two
letter acronyms (for example, `IO`).

### 4.iv.&emsp;Use the singular for module names

Use the singular when naming modules; e.g., use `Data.Map` and
`Data.ByteString.Internal` instead of `Data.Maps` and
`Data.ByteString.Internals`.

5.&emsp;Strictness
------------------

By default, use strict data types and lazy functions.

### 5.i.&emsp;Make data types strict by default

Constructor fields should be strict unless there is an explicit reason
to make them lazy. This avoids many common pitfalls caused by too much
laziness and reduces the amount of time the programmer has to spend
thinking about evaluation order.

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

Note that `-funbox-strict-fields` applies to all strict fields, not just
small fields (e.g., `Double` or `Int`). If you are using GHC 7.4 or
later, you can use `NOUNPACK` to selectively opt-out for the unpacking
enabled by `-funbox-strict-fields`.

### 5.ii.&emsp;Make function arguments lazy by default

Make function arguments lazy unless you explicitly need them to be
strict.

The most common case when you need strict function arguments is in
recursion with an accumulator:

```haskell
mysum :: [Int] -> Int
mysum = go 0
  where
    go !acc []      = acc
    go acc (x : xs) = go (acc + x) xs
```

6.&emsp;Miscellaneous
---------------------

### 6.i.&emsp;Avoid over-using point-free style

For example, `f = (g .) . h` is harder to read than `f x = g . h x`.

### 6.ii.&emsp;Code must be warning-free

Code should not produce warnings when compiled with `-Wall`.

### 6.iii.&emsp;Do not escape newlines in strings

Escaped newlines will make using the `CPP` language extension impossible.
