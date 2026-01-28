---
layout: post
title: "Xbox One controller Bluetooth on Arch Linux"
date: 2026-01-27
---

**I spent some time** getting my Xbox One Wireless Controller to work with Bluetooth on Arch, so I'll put the solution here, in case it helps someone else in the future.

I could connect to the Bluetooth device, but I wasn't getting any inputs in games I tried to play.

This did the trick:

```sh
sudo pacman -S dkms linux-headers
yay -S xpadneo-dkms
```

Then reboot and connect over Bluetooth as normal.
