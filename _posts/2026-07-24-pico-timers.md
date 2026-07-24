---
layout: post
title: "Setting timers in simple games (feat. the frame rule)"
date: 2026-07-24
---

Lately I've been reading [the disassembled source code to Super Mario Bros.](https://6502disassembly.com/nes-smb/SuperMarioBros.html), and I learned that events are scheduled for the future in much the same way I do it when making a quick game-jam game in Lua, such as [o moku e mun](https://rose.systems/omokuemun/).

It's so simple it's barely a system! But I like how understandable-yet-flexible it is. I'll describe it in Lua, for use in a PICO-8 or Picotron game. It's two lines of code! Read 'em and weep.

## Timers in PICO-8

First, define a table `T`. I'd like to add some Lua magic that makes `T.foo` be 0 if it hasn't been set yet. This is a bit strange, but in a game jam it saves some precious time bouncing to the top of the file and back to add `T.foo=0` and prevent `nil` errors. Here it is:

```lua
local T = setmetatable({}, {__index = function() return 0 end})
```

Then, in the `_update` function (once every frame), tick down all the timers:

```lua
for k,v in pairs(T) do T[k]=max(0,v-1) end
```

That's it! At any time, `T.foo` is the amount of frames left on the `foo` timer.

### 1. Making stuff happen, for a while

Let's say you want the player to glow for one second. That's 60 frames.

You just write `T.glow=60` to start the timer, and elsewhere you check `T.glow>0` to decide whether to draw a glowy sprite.

To stop the glowing, you write `T.glow=0`. To extend the glow, you trigger `T.glow=60` again. Simple!

### 2. Making stuff happen, later

Let's say you want to play a *blip!* sound effect, two seconds from now.

You set `T.blip=120` to start the timer, and then in your update function you write: `if (T.blip==1) sfx(2)`.

To cancel the timer, just set `T.blip=0`.

You can also write stuff like `if (T.blip%10==1) sfx(2)` to blip repeatedly while the timer is running.

### 3. Non-global timers

You can also have more than one `T`. Maybe your `Enemy` objects each have their own `self.T`. Just make sure your `Enemy:update` ticks everything in the table down.

## Timers in Super Mario Bros.

Now, there are no fancy key-value tables when you're programming an NES game. The timers are, instead, all stored contiguously in RAM, and [this loop](https://6502disassembly.com/nes-smb/SuperMarioBros.html#SymDecTimersLoop) counts them all down.

Each timer can only hold a single-byte value, and the NES runs at 60 fps, so we can't set timers for very long this way. `0xff` frames is only 4.25 seconds. To work around this this, about half the timers are programmed to only tick down every time a kind of "higher-up timer," called _IntervalTimerControl_ in the disassembly, tells them to.

Translated to pseudo-Lua, it's like this:

```lua
itc = 0

-- Called every frame
function DecTimers()
  last_timer = 20
  itc = itc - 1
  if itc < 0 then
    itc = 20  -- 20,19,18...0 = 21 frame clock
    last_timer = 35
  end

  -- Tick those timers!
  for i = 0, last_timer do
    if timers[i] > 0 then
      timers[i] = timers[i] - 1
    end
  end
end
```

Most of the time, `last_timer` is 20, and so this function ticks only the "fast" timers `timer[0]` through `timer[20]`. But once every 21 frames,[^1] _IntervalTimerControl_ expires, and *all* timers, `timer[0]` through `timer[35]`, get ticked down.

In particular, _IntervalTimerControl_ is reset once per level, and a "slow" timer is used to time the animation at the end of the level. This is the origin of the famous [frame rule](https://en.wikipedia.org/wiki/Super_Mario_Bros._speedrunning#Frame_rules) in Super Mario Bros. speedrunning! Your gameplay can't influence the value of _IntervalTimerControl_, and the level-end animation timer is synchronized to it, so you can mostly only improve your level times by 21-frame increments.