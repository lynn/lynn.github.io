# Code golf diary
I play [code golf]() (as "lynn") on [golf.shinh.org](http://golf.shinh.org/) and [code.golf](https://code.golf/), mostly in Python and Haskell.

I'll do little post-mortem writeups of problems here. Nothing but spoilers here!

## 2021-03-13 [even 2](http://golf.shinh.org/p.rb?even+2)
This is a strange little "map a few short inputs to a few short outputs" kind of challenge.

### My Python solution
I tied the shortest Python solution, after finding a bunch of one-byte-longer ones:
```py
print 2,4,6,1<<input()-5   # 24
print 2,4,6,input()/8*'8'  # 25
print 2,4,input(6)/8*' 8'
print 2,'4 6 8'[:input()]
print 2,' 6 8'[:input(4)]
print 2,4,6,input()&8or''
```
`1<<input()-5` is 8 when the input is 8, but crashes when the input is 3 (a negative shift like `1<<-2` causes an error). Thankfully, `2,4,6` safely make it to stdout.

### kops' Vim solution
kops found an amazing [11-byte Vim solution](http://golf.shinh.org/reveal.rb?even+2/kops_1615070278&vi). It starts with `P`aste! It turns out that if you cut text in one test case, it's still in your `@0` register in the next one.

For the first test case, it `P`astes nothing, then uses `s` to replace the only character in the input with `2 4 6 `. (Trailing whitespace is trimmed when checking your output.)

For the second test case, it `P`astes the character that `s` copied in the last test case: now the buffer looks like `38`. Then, `s` replaces the `3` with `2 4 6 `, leaving `2 4 6 8`.

## 2021-03-13 [Rotate letters FIXED](http://golf.shinh.org/p.rb?Rotate+letters+FIXED)
Everyone got 62 in Python.
```py
import re
t=re.sub(' (.)','\\1 ',raw_input())
print t[1:]+t[0]
```

I got 59 in Haskell.
```hs
f(x:y)=x:' ':y
g(x:_:z)=z++[x]
main=interact$g.(>>=f).words
```

The Ruby solution looks sort of magical.
