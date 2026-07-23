---
layout: post
title: "TOMLing my Discord servers"
date: 2026-07-23
---

<style>
.how { padding: 1rem; background: light-dark(rgb(255, 227, 214, 1), rgb(62, 26, 58, 1)); position: relative; }
.how :first-child { margin-top: 0; }
.how :last-child { margin-bottom: 0; }
/* .how pre, .how code { background-color: var(--color-bg); } */
</style>

I am in far too many Discord servers. I wanted to organize the folders they're grouped in. This is really annoying to do in Discord itself, because it only lets you drag and drop them around one at a time in the left sidebar. So I did a little bit of messing around in Firefox's Developer Tools to extract and reinsert my folder data.

I'm not sure if I saved time doing all this. It would have been faster to move 80 little circles around, if a bit mind-numbing. On the plus side, I learned a thing or two about Discord's internals, and I can play around with various ways to group my servers now to see which one I like best: all I have to do is re-run a script.

Another thought I had is that it's nice to show a bit of the behind-the-scenes thinking for this kind of "soft reverse-engineering" task. So I'm blogging about it!

I'll call Discord servers _guilds_ for the rest of this post, because that's what they are called internally.

{:.how}
**Want to play along at home?** Open Discord in your browser. Open the Developer Tools (F12) and refresh. Follow the instructions in the boxes like this one. You'll need the Python package manager [uv](https://docs.astral.sh/uv/), which is excellent at running Python scripts with dependencies. (It installs them on the fly when you run `uv --with blah`.)

---

## Getting our bearings

I figured Discord must be sending *some* request to an API *somewhere* when I rearrange guilds in the sidebar. I know Discord uses WebSockets, in general, so that's were I looked first. I tried rearranging guilds in my sidebar and seeing if a message fires on the socket. But nothing happened. Mysterious!

Then I noticed this helpful pair of lines in the JavaScript console logs:

```
[discord_protos.discord_users.v1.PreloadedUserSettings] Updating guildFolders with delay 10
[discord_protos.discord_users.v1.PreloadedUserSettings] Scheduling save from markDirty
```

Aha, "scheduling save" and "delay 10"! So maybe it's [debounced](https://developer.mozilla.org/en-US/docs/Glossary/Debounce) and only saves the new order after 10 seconds? I counted to 10 after rearranging my guilds, and still didn't see anything relevant on the socket. But then I noticed it makes an HTTP PATCH request, namely to:

> _https://discord.com/api/v9/users/@me/settings-proto/1_

The request body is, at first blush, impenetrable: `{"settings": "cqIJ..."}` with some base-64-encoded binary data. I figured `proto` probably means [Protocol Buffers](https://en.wikipedia.org/wiki/Protocol_Buffers), which means I can't decode the request body without a schema. Rats!

At this point I just searched for "settings-proto discord" online and found [unofficial docs](https://docs.discord.food/resources/user-settings-proto) for this internal API. So handy! They even linked to a [Python package](https://www.npmjs.com/package/discord-protos) for parsing these protos, wow. Their examples will make parsing this Protocol Buffer data easy.

Let's save the request, hoping it contains all information about the new order (spoiler: it does).

<div class="how" markdown="1">
## Downloading guild folder data

In the Network tab, filter requests for `@me/`. Rearrange any two guilds in the sidebar and wait 10 seconds. A PATCH request should appear:

<img height="300" alt="image" src="https://gist.github.com/user-attachments/assets/3193b72a-35d3-416a-9f23-4fbe331bf92b" />

Click it, then click "Request" on the right. Right-click and "Copy All":

<img height="220" alt="image" src="https://gist.github.com/user-attachments/assets/e1ad3739-9a4d-4829-8dc1-85386f3a2ddb" />

Save this as `folder.json`.

</div>

At this point I started playing around with decoding this data, loosely following the example from the discord-protos Python package.

<div style="letter-spacing:-0.5px;" markdown="1">
<pre>
<span class=c>$ uv run --with discord-protos python3
Python 3.11.14 (main, Oct 14 2025, 21:26:53) [Clang 20.1.4 ] on linux
Type "help", "copyright", "credits" or "license" for more information.</span>
&gt;&gt;&gt; import json
&gt;&gt;&gt; j = json.loads(open("folders.json").read())
&gt;&gt;&gt; j['settings'][:20]  <span class=c># it's base64 data</span>
<span class=c>'cu8JCgoKCAAwQD/WmpAB'</span>
&gt;&gt;&gt; from google.protobuf.json_format import MessageToDict
&gt;&gt;&gt; from discord_protos import PreloadedUserSettings
&gt;&gt;&gt; from base64 import b64decode
&gt;&gt;&gt; b = b64decode(j['settings'])
&gt;&gt;&gt; MessageToDict(PreloadedUserSettings.FromString(b))
<span class=c>{'guildFolders': {'folders': [{'guildIds': ['112760235659112448']}, ...</span>
</pre>
</div>

Alright! But a list of numeric IDs will be hard to edit. Can I get all the names corresponding to these guild IDs somehow?

---

## Obtaining guild names

I wasn't totally sure where to start looking for this, and briefly tried to extract this information from the page using JavaScript in Developer Tools. But then I thought to check the unofficial docs I had just found.

Indeed, there is a `/@me/guilds` endpoint, documented [here](https://docs.discord.food/resources/guild#list-user-guilds). I could just send a request there and get a JSON object with all the information I need. Yay!

"Edit and Resend" in the Developer Tools is really handy here. I used to always "copy as cURL" and then mess with the huge cURL command before sending it off, but this is, I think, nicer.

<div class="how" markdown="1">
### Procedure

In the Network tab, filter requests for `@me/` and find any "GET" request:

<img height="80" alt="image" src="https://gist.github.com/user-attachments/assets/18ba8e9f-6a44-480e-b293-1682d1a65707" />

Right-click this request, choose "Edit and Resend", and change everything after `@me/` to `guilds`:

<img height="110" alt="image" src="https://gist.github.com/user-attachments/assets/8da3f850-c05f-4684-8350-4188bc9f776a" />

On the right, under the "Response" tab, you will see a big JSON response, with info about all the guilds you're in. Right-click it and "Copy All":

<img height="120" alt="image" src="https://gist.github.com/user-attachments/assets/797d2236-d1a4-4f12-be27-90b574058a68" />

Paste this into a text editor and save it as `guilds.json` somewhere.
</div>

## Turning folders into an easily editable text file

From here on it was just a matter of writing some useful scripts. I think the nicest JSON-like to edit by hand is TOML, so I figured I'd transform `folders.json` into a TOML file that has my guild names in the comments. Something like this:

```toml
guildPositions = [
    # This stuff is marked as "deprecated" in the docs I found.
    # I guess I won't touch it.
    "...",
    "...",
    "...",
]

[[folders]]
guildIds = [
    "756829356155731988",  # ⛳ Code Golf
    "1285973048092000388", # Byte Heist
]
id = "1234512345"
name = "codegolf"

[[folders]]
guildIds = [
    "1208593165213245510", # ma mun
    "1337041613096222730", # toki pona en toki Netelan
    "861015480197447710",  # toki wile
]
id = "3652584801"
name = "tp"
```

Did you know Python has a [built-in TOML reader](https://docs.python.org/3/library/tomllib.html) but not a writer? Funny. I used [Tomli-W](https://pypi.org/project/tomli-w/) to write TOML.

<div class="how" markdown="1">
## Extract TOML from base64 JSON folder update
Save the following Python script as `extract.py` next to your `.json` files:

```py
from discord_protos import PreloadedUserSettings
from base64 import b64decode
from google.protobuf.json_format import MessageToDict
import tomli_w, json, re

with open("folders.json") as f: raw_folders = json.load(f)
with open("guilds.json") as f: guilds = json.load(f)

id_to_name = {g["id"]: g["name"] for g in guilds}
b = b64decode(raw_folders["settings"])
folders = MessageToDict(PreloadedUserSettings.FromString(b))

dump = tomli_w.dumps(folders["guildFolders"])
f = lambda m: m[0].ljust(23) + "# " + id_to_name.get(m[1], "")
print(re.sub(r'"(\d{5,})",', f, dump))
```

Now open a command line in this folder and run

```sh
uv run --with discord-protos,tomli-w python3 extract.py > folders.toml
```

I think you can make up numbers for the `id` fields if you make new folders. Oh, and just leave the "guildPositions" section at the top alone. I think it does nothing.
</div>

At this point, I edited the TOML file to test my approach: first, just merging all my existing folders into one big "other" folder. Which means I now had to write the other script...

<div class="how" markdown="1">

## Applying changes

Save this Python script as `reimport.py` next to your `.json` files:

```py
from discord_protos import PreloadedUserSettings
from base64 import b64decode, b64encode
from google.protobuf.json_format import MessageToDict, ParseDict
import tomllib, json

with open("folders.toml", "rb") as f: folders_dict = tomllib.load(f)
folders = ParseDict({'guildFolders': folders_dict}, PreloadedUserSettings())
b64 = b64encode(folders.SerializeToString()).decode('ascii')
print(json.dumps({"settings": b64}))
```

Now run this in the command line:

```py
uv run --with discord-protos python3 reimport.py
```

It will spit out something like `{"settings": "cqIJCgoKCA..."}` — copy all this (you may need Ctrl+Shift+C to copy from the command line).

Now go back to the "PATCH" request in your browser. Right click, choose "Edit and Resend", then select the stuff at the bottom and paste to replace it:

<img height="450" alt="image" src="https://gist.github.com/user-attachments/assets/775257a5-a80f-4e3f-90b5-ffdc425df043" />

</div>

It works! So now I'll just edit `folders.toml` to suit my needs and re-run `reinsert.py`. I'd show off the results, but I feel like they are a bit too private.
