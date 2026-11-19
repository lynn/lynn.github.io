"""Download and parse the French original and the English (Katherine Woods) text
into fr.json / en.json: a list of chapters (0 = dedication), each a list of paragraphs.

Usage: python3 align/sources.py
"""
import html, json, os, re, urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = os.path.join(HERE, 'cache')
FR_URL = 'https://www.ebooksgratuits.com/html/st_exupery_le_petit_prince.html'
EN_URL = 'https://www.odaha.com/antoine-de-saint-exupery/maly-princ/the-little-prince'


def fetch(url, name):
    os.makedirs(CACHE, exist_ok=True)
    p = os.path.join(CACHE, name)
    if not os.path.exists(p):
        req = urllib.request.Request(url, headers={'User-Agent': 'Mozilla/5.0'})
        with urllib.request.urlopen(req) as r, open(p, 'wb') as f:
            f.write(r.read())
    return open(p, encoding='utf-8', errors='replace').read().replace('\r', '')


def clean(t):
    t = html.unescape(re.sub(r'<[^>]+>', '', t)).replace('\xa0', ' ')
    return re.sub(r'\s+', ' ', t).strip()


def french():
    s = fetch(FR_URL, 'fr.html')
    start = s.rindex('<p', 0, s.index('À LÉON WERTH</span></p>'))
    s = s[start:s.index('<div class=Section2>')]
    chapters = [[]]
    for m in re.finditer(r'<h1.*?</h1>|<p[^>]*>(.*?)</p>', s, re.S):
        if m.group(0).startswith('<h1'):
            chapters.append([])
            continue
        t = clean(m.group(1))
        # An image caption that this edition repeats at the end of chapter II.
        if t and not t.startswith('Voilà le meilleur portrait que, plus tard'):
            chapters[-1].append(t)
    return chapters


def english():
    s = fetch(EN_URL, 'en.html')
    s = s[s.index('class="entrytext"'):]
    # The web text lacks the dedication.
    chapters = [[
        'TO LEON WERTH',
        'I ask the indulgence of the children who may read this book for dedicating it to a grown-up. '
        'I have a serious reason: he is the best friend I have in the world. I have another reason: '
        'this grown-up understands everything, even books about children. I have a third reason: he '
        'lives in France where he is hungry and cold. He needs cheering up. If all these reasons are '
        'not enough, I will dedicate the book to the child from whom this grown-up grew. All grown-ups '
        'were once children—although few of them remember it. And so I correct my dedication:',
        'TO LEON WERTH',
        'WHEN HE WAS A LITTLE BOY',
    ]]
    pat = r'<img[^>]*src="[^"]*/chapter(\d+)\.gif"|<div class="story[^"]*"[^>]*>(.*?)</div>'
    for m in re.finditer(pat, s, re.S):
        if m.group(1):
            chapters.append([])
            continue
        t = clean(m.group(2)).replace('--', '—')
        t = re.sub(r'(^|[\s(])"|(—)"(?=\w)', r'\1\2“', t)
        t = t.replace('"', '”').replace("'", '’')
        if t:
            chapters[-1].append(t)
    return chapters


for name, chapters in (('fr', french()), ('en', english())):
    with open(os.path.join(HERE, name + '.json'), 'w', encoding='utf-8') as f:
        json.dump(chapters, f, ensure_ascii=False, indent=1)
    print(f'{name}.json: {len(chapters)} chapters, {sum(map(len, chapters))} paragraphs')
