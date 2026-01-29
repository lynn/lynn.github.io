---
layout: default
---

I'm **Lynn** / chordbug!
I'm a programmer and musician from Antwerp, Belgium. I'm trans, and my pronoun is *she*.
Lately I microblog on [Bluesky](https://bsky.app/profile/chordbug.bsky.social).

I like math, linguistics, conlangs, old sound chips, interactive fiction, retro computers, typography, stenotype, and cute anime art.
Sometimes I dabble in drawing, writing, or [gamedev](https://0xlynn.itch.io/).
You can find some of my music on [Bandcamp](https://chordbug.bandcamp.com/) and [SoundCloud](https://soundcloud.com/chirptune) and [Twitter](https://twitter.com/search?q=from%3Achordbug%20video&src=typed_query).

As a programmer, I like to daydream about accessibility, localization, Unicode, type systems, monads, UX design, and the <ruby><rb><tt>&lt;ruby&gt;</tt></rb><rp> (</rp><rt style="margin-bottom:-6px;margin-top:-3px;">yeah!</rt><rp>)</rp></ruby> tag.
At times, I get really into [code golf](https://code.golf/rankings/holes/all/all/bytes).
My nicer code is [on GitHub](http://github.com/lynn).

Here are some projects of mine:

- [*autokalimba*](harp), a musical instrument for your phone ([read more](https://github.com/lynn/harp))
- an [*English translation of Sword of Kumdor*](kumdor), a touch-typing JRPG
- [*o moku e mun*](https://roachbones.itch.io/o-moku-e-mun), a roguelike game in Toki Pona
- [*koakuma*](https://github.com/lynn/koakuma), an anime art trivia game bot
- A [fork of *chibicc*](https://github.com/lynn/chibicc) for compiling C code to [uxn](https://100r.co/site/uxn.html)
- [*pysearch*](https://github.com/lynn/pysearch), a program that finds small Python expressions

If you like what I do, you can [buy me a coffee](https://ko-fi.com/chordbug). <span aria-hidden="true">♡</span>

Thanks for stopping by.

---

## Blog

{% if site.posts.size > 0 %}
{% for post in site.posts %}
- {{ post.date | date: "%Y-%M-%d" }} [{{ post.title }}]({{ post.url }})
{% endfor %}
{% else %}
No posts yet.
{% endif %}
