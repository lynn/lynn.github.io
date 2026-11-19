"""Align the paragraphs of content.md with the French original, and the French with
the English translation, chapter by chapter. Writes ../translations.json: a list of
chapters, each a list (one entry per content.md paragraph, `*…*` lines included) of
{"fr": [...], "en": [...]}.

Gale–Church-style dynamic programming over paragraph lengths, plus a dialogue-marker
match and (for French↔English) a crude cognate overlap score.

Usage: python3 align/align.py   (after align/sources.py)
"""
import json, math, os, re, sys, unicodedata

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
fr = json.load(open(os.path.join(HERE, 'fr.json'), encoding='utf-8'))
en = json.load(open(os.path.join(HERE, 'en.json'), encoding='utf-8'))

# (chapter, English paragraph) -> French paragraph, where the automatic alignment goes wrong.
EN_OVERRIDE = {(4, 10): 6, (4, 12): 8}


def lojban_chapters():
    md = open(os.path.join(ROOT, 'content.md'), encoding='utf-8').read()
    blocks = [re.sub(r'\s*\n\s*', ' ', b.strip()) for b in re.split(r'\n\s*\n', md) if b.strip()]
    out = []
    for b in blocks:
        if b.startswith('## '):
            out.append([])
        elif out and not b.startswith(('# ', '### ', '![')):
            out[-1].append(b)
    return out


def plain_jb(t):
    return re.sub(r'^ni.o ', '', t.strip('*'))


def isdlg_fr(t): return t[:1] in '–—-'
def isdlg_jb(t): return t.startswith('— ')
def isdlg_en(t): return t[:1] in '"“'


def stems(t):
    t = unicodedata.normalize('NFD', t.lower())
    t = ''.join(ch for ch in t if not unicodedata.combining(ch))
    return {w[:4] for w in re.findall(r'[a-z0-9]+', t) if len(w) >= 4 or w.isdigit()}


# (source paragraphs, target paragraphs, penalty)
BEADS = [(1, 1, 0), (0, 1, 1.5), (1, 0, 1.5), (2, 2, 3), (2, 3, 3.5), (3, 2, 3.5)] + [
    x for k in range(2, 7) for x in ((1, k, 0.6 + 0.9 * (k - 1)), (k, 1, 0.6 + 0.9 * (k - 1)))]


def align(A, B, ratio, dA, dB, lex=False):
    """Returns a list of ((i0, i1), (j0, j1), cost) beads covering A and B."""
    n, m = len(A), len(B)
    LA = [len(a) for a in A]; LB = [len(b) for b in B]
    SA = [stems(x) for x in A]; SB = [stems(x) for x in B]
    INF = 1e18
    D = [[INF] * (m + 1) for _ in range(n + 1)]
    P = [[None] * (m + 1) for _ in range(n + 1)]
    D[0][0] = 0
    for i in range(n + 1):
        for j in range(m + 1):
            if D[i][j] == INF:
                continue
            for a, b, pen in BEADS:
                i2, j2 = i + a, j + b
                if i2 > n or j2 > m:
                    continue
                la = sum(LA[i:i2]); lb = sum(LB[j:j2])
                if a and b:
                    c = pen + 4 * math.log((lb + 20) / (la * ratio + 20)) ** 2
                    if dA(A[i]) != dB(B[j]): c += 1.5
                    if dA(A[i2 - 1]) != dB(B[j2 - 1]): c += 0.5
                    if lex:
                        sa = set().union(*SA[i:i2]); sb = set().union(*SB[j:j2])
                        if sa and sb:
                            c -= 3 * len(sa & sb) / min(len(sa), len(sb)) * min(1, (len(sa) + len(sb)) / 8)
                else:
                    c = pen + 0.006 * (la + lb)
                if D[i][j] + c < D[i2][j2]:
                    D[i2][j2] = D[i][j] + c
                    P[i2][j2] = (i, j, c)
    out = []; i, j = n, m
    while (i, j) != (0, 0):
        pi, pj, c = P[i][j]
        out.append(((pi, i), (pj, j), c))
        i, j = pi, pj
    return out[::-1]


jb = lojban_chapters()
if len(jb) != len(fr) or len(jb) != len(en):
    sys.exit(f'chapter counts differ: jbo {len(jb)}, fr {len(fr)}, en {len(en)}')
total = lambda x: sum(len(p) for c in x for p in c)
r_jb = total([[plain_jb(p) for p in c] for c in jb]) / total(fr)
r_en = total(en) / total(fr)

result = []
for c in range(len(jb)):
    J = [plain_jb(p) for p in jb[c]]
    aj = align(fr[c], J, r_jb, isdlg_fr, isdlg_jb)
    ae = align(fr[c], en[c], r_en, isdlg_fr, isdlg_en, lex=True)
    # French paragraphs with no Lojban counterpart join the previous bead.
    folded = []
    for b in aj:
        if b[1][0] == b[1][1] and folded:
            (i0, _), js, cost = folded[-1]
            folded[-1] = ((i0, b[0][1]), js, cost)
        else:
            folded.append(b)
    # English paragraph -> set of French paragraphs.
    enfr = {}
    for (a0, a1), (k0, k1), _ in ae:
        for k in range(k0, k1):
            enfr[k] = set(range(a0, a1))
    for (cc, k), v in EN_OVERRIDE.items():
        if cc == c:
            enfr[k] = {v}
    # English paragraphs with no French counterpart join the previous one.
    for k in range(len(en[c])):
        if not enfr.get(k):
            enfr[k] = enfr.get(k - 1, {0})
    chapter = []
    for (i0, i1), (j0, j1), cost in folded:
        frs = set(range(i0, i1))
        if not frs:
            print(f'warning: chapter {c} paragraph {j0} has no French', file=sys.stderr)
        for j in range(j0, j1):
            chapter.append({
                'fr': [fr[c][i] for i in sorted(frs)],
                'en': [en[c][k] for k in range(len(en[c])) if enfr[k] & frs],
            })
    result.append(chapter)

with open(os.path.join(ROOT, 'translations.json'), 'w', encoding='utf-8') as f:
    json.dump(result, f, ensure_ascii=False, indent=1)
print(f'translations.json: {sum(map(len, result))} paragraphs')
