---
layout: post
title: "Days Since `pacman -Syu`"
date: 2026-01-29
---

**Sometimes I wake** up in a cold sweat and realize it's been three weeks since I ran `pacman -Syu`. No more. I use GNOME btw, so I made [a little GNOME shell extension](https://github.com/lynn/days-since-update) for myself, constantly showing how long it's been in the top bar.

![Screenshot of the indicator in the top bar](/images/days-since-update.png)

If it's been ten days or more, the number turns yellow.

As far as engineering goes, the process was pretty unremarkable. LLMs make it easier than ever to turn the sparkle of an idea into a two-hour project. The GNOME extensions project has a reasonable [no-vibe-coding policy](https://www.theverge.com/news/844655/gnome-linux-ai-shell-extensions-ban), so I took care to tend to the business logic by hand and trim down the generated fluff.

Lately, a not-too-serious project like this feels like an opportunity to leave a handwritten mark on a program: a silly variable name or a little code golf trick. Did you know there are `864e5` milliseconds in a day?

You can install _Days Since Update_ [here](https://extensions.gnome.org/extension/9222/days-since-update/).

