---
layout: post
title: "Reader-style Uiua"
date: 2026-01-24
---

Lately I've been playing with [Uiua](https://www.uiua.org/), an APL-like programming language that has no local variables on purpose. Instead, you describe how data flows through your function using combinators like "fork" (call two functions on the same value, keeping both results) and "on" (call a function, but also keep the value around to do something else). You can think of the language as stack-based, though the official documentation has recently started to discourage this perspective.

I appreciate the die-hard commitment to tacit, point-free programming on an aesthetic level, but on a practical level I find it often gets in the way. I thought of a nice way to make programming in Uiua feel more like a regular stack-based language, though. We can write functions that keep the top-of-stack fixed to some ambient "context" that we constantly either read parts from, or dip past.

Here's an example of a "quadratic discriminant" function, `Disc(C,B,A) = B² - 4AC`, side-by-side with what we might write if Uiua had local variables:

```ua
Disc ← (          # Disc(C_B_A) ← (
  ~ {C B A}       #
  ⊙ⁿ₂ ⟜B          #   ⁿ₂ B
  ⊙× ⊙4 ⊙× ⟜A ⟜C  #   × 4 × A C
  ⊙-              #   -
  ◌               #
)
```

We define the shape of the context using a local data definition. This context stays on top, and everything underneath it is the _effective stack_. Every time we want to read from our context, we use `⟜Accessor` to push it to the top of the effective stack. To manipulate the effective stack, we use `⊙` liberally to dip past the context. At the end, we pop `◌` the context, leaving only the effective stack values.

I want to call this style "reader-style", because in a way, this is like programming in a Reader monad: `⟜f` is like [`asks f`](https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-Reader.html#v:asks) and `⊙f` is like `pure`/`fmap`/`liftA2` of `f`. For comparison, here is an equivalent Haskell definition of `disc`, which is also "point-free without really being point-free."

```hs
import Control.Monad.Reader
import Control.Applicative

data Quadratic = Quadratic { c, b, a :: Float } deriving (Eq, Show)

disc :: Reader Quadratic Float
disc =
  liftA2 (-)
    (fmap (^2) (asks b))
    (liftA2 (*) (pure 4) $ liftA2 (*) (asks a) (asks c))
```

