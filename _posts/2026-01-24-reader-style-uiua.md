---
layout: post
title: "Reader-style Uiua"
date: 2026-01-24
---

**Lately I've been** playing with [Uiua](https://www.uiua.org/), an APL-like programming language that has no local variables on purpose. Instead, you describe how data flows through your function using combinators. One can also think of the language as stack-based, though the official documentation has recently started to discourage this perspective.

I appreciate the die-hard commitment to tacit, point-free programming on an aesthetic level, but on a practical level I find it often gets in the way. I've landed on a nice way to make programming in Uiua feel more like a regular stack-based language. We can write functions that keep the top-of-stack fixed to some ambient passed-in **context**, that we constantly either read parts from, or dip past.

Here's an example of a [quadratic discriminant](https://en.wikipedia.org/wiki/Quadratic_equation#Discriminant) function, `Disc(c,b,a) = b²−4ac`, side-by-side with what we might write if Uiua had local variables:

```ua
Disc ← (           # Disc(C_B_A) ← (
  ~ {C B A}        #
  ⊙ⁿ₂ ⟜B           #   ⁿ₂ B
  ⊙× ⊙4 ⊙× ⟜A ⟜C   #   × 4 × A C
  ⊙-               #   -
  ◌                #
)
```

We define the shape of the context using a local [data definition](https://www.uiua.org/tutorial/Data%20Definitions) `~`. This context stays on top, and everything underneath it is the **effective stack**, which starts empty.

Now here's how we write our code:

1. Every time we want to read from our context, we use `⟜F` to push `F(context)` to the top of the effective stack.
2. To manipulate the effective stack any other way, we use `⊙` to dip past the context.
3. At the end, we pop `◌` the context, leaving only the effective stack values.

I want to call this style **reader style**, because in a way, this is like programming in a Reader monad: `⟜f` is like [`asks f`](https://hackage-content.haskell.org/package/mtl-2.3.2/docs/Control-Monad-Reader.html#v:asks) and `⊙` is like `pure` or `fmap` or `liftA2`. For comparison, here is an equivalent Haskell definition of `disc`, which is also "point-free without really being point-free."

```hs
import Control.Monad.Reader
import Control.Applicative

data Quadratic =
  Quadratic { c, b, a :: Float } deriving (Eq, Show)

disc :: Reader Quadratic Float
disc =
  liftA2 (-)
    (fmap (^2) (asks b))
    (liftA2 (*) (pure 4) $ liftA2 (*) (asks a) (asks c))
```

This style can and should be mixed freely with regular flavors of Uiua. For example, `⊙× ⊙4 ⊙× ⟜A ⟜C` could be written `⟜(×₄×⊃A C)`, where we use a small, easy-to-read tacit function as our accessor function.

<script>
  const uiua = document.getElementsByClassName("language-ua")[0];
  uiua.innerHTML = uiua.innerHTML.replace(/#.*/g, "<span style=opacity:0.5>$&</span>")
</script>