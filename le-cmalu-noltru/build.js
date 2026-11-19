#!/usr/bin/env node
// Static site build: content.md + template.html -> index.html
//
//   node build.js
//
// Every Lojban paragraph is parsed with camxes (camxes.js, built from
// grammar/camxes-beta.peg). From the parse:
//   - the selbri of each bridi_tail_3 is wrapped in <u> (shown when <body> has
//     class "selbri"; its "selbrisni linji" checkbox is currently disabled),
//   - lujvo with certain rafsi get a margin note with their veljvo (see NOTE_RAFSI),
//   - each subsentence is wrapped in [{(⟨ brackets by nesting depth (shown when
//     the reader ticks "brisni bitmu barna"),
//   - quoted text (zo X, lu ... li'u, and paragraphs starting with "—") gets a
//     <span class="q"> background, except for sei clauses inside quotes,
//   - paragraphs that fail to parse get class="bad", with the parser's error
//     as a tooltip (quotes in them are then found with regexes instead).
//
// content.md is a small Markdown subset:
//   # title          ## chapter (e.g. "## pa mo'o")       ### closing line
//   *italic line*    (a whole paragraph in *...*: byline before the first chapter, subtitle after;
//                     may contain [links](url))
//   ![](src){width=70%}
//   Anything else is a paragraph; paragraphs are separated by blank lines.
//   Paragraphs starting with "— " are dialogue.

const fs = require('fs');
const path = require('path');
const vm = require('vm');

const dir = __dirname;
const read = (f) => fs.readFileSync(path.join(dir, f), 'utf8');

// camxes.js is a browser script defining a global `camxes`; load it in a sandbox.
const sandbox = {};
vm.createContext(sandbox);
vm.runInContext(read('camxes.js') + '\nthis.camxes = camxes;', sandbox);
const camxes = sandbox.camxes;

// latkerlo jvotci (lujvo tools), also browser scripts.
const jvotci = {};
vm.createContext(jvotci);
for (const f of ['data', 'rafsi', 'tarmi', 'tools', 'jvozba', 'katna']) {
  vm.runInContext(read(`latkerlo-jvotci/${f}.js`), jvotci, { filename: `${f}.js` });
}

// Lujvo containing one of these rafsi get a margin note with their veljvo,
// at their first occurrence.
const NOTE_RAFSI = new Set(("ted cir jge ful sa'o dzi jic gre tap sab pum fi'u ku'e cu'u ca'i " +
  "tro ke'a plo ril jab ne'o ru'e ka'u").split(' '));
const noted = new Set();
function lujvoNote(word) {
  word = word.toLowerCase();
  if (noted.has(word)) return null;
  let rafsi, veljvo;
  try {
    jvotci.word = word;
    [rafsi, veljvo] = vm.runInContext('[jvokaha(word), getVeljvo(word)]', jvotci);
  } catch (e) {
    return null; // not decomposable (e.g. sincrboua)
  }
  if (!rafsi.some((r) => NOTE_RAFSI.has(r))) return null;
  noted.add(word);
  return veljvo.join(' ');
}

const esc = (s) => s.replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]));

// ---------------------------------------------------------------- parsing --

const DIGITS = ['no', 'pa', 're', 'ci', 'vo', 'mu', 'xa', 'ze', 'bi', 'so'];
const ACCENTS = { á: 'A', à: 'A', â: 'A', ä: 'A', é: 'E', è: 'E', ê: 'E', ë: 'E',
  í: 'I', ì: 'I', î: 'I', ï: 'I', ó: 'O', ò: 'O', ô: 'O', ö: 'O',
  ú: 'U', ù: 'U', û: 'U', ü: 'U', ý: 'Y', ỳ: 'Y', ŷ: 'Y', ÿ: 'Y' };

// A cut-down version of ilmentufa's camxes_preprocessing that keeps track of
// which original character each character of the parser input came from.
function preprocess(text) {
  let input = '';
  const map = [];
  const emit = (s, i) => { for (const c of s) { input += c; map.push(i); } };
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (c === '’') emit("'", i);
    else if (c === '·') emit('.', i);
    else if (c === '­') {}
    else if (c >= '0' && c <= '9') emit(DIGITS[+c], i);
    else if (ACCENTS[c]) emit(ACCENTS[c], i);
    else if ('()«»‹›—:'.includes(c)) emit(' ', i);
    else emit(c, i);
  }
  map.push(text.length);
  return { input, map };
}

// The patched camxes labels some nodes "label@START-END" (see
// grammar/pegjs_header.js). Returns { name, start, end } or null.
function tag(node) {
  if (!Array.isArray(node) || typeof node[0] !== 'string') return null;
  const m = /^(\w+)@(\d+)-(\d+)$/.exec(node[0]);
  return m && { name: m[1], start: +m[2], end: +m[3] };
}

// Labelled children of a node, looking through unlabelled grouping arrays.
function children(node) {
  const out = [];
  const visit = (xs) => {
    for (const c of xs) {
      if (!Array.isArray(c)) continue;
      if (typeof c[0] === 'string') out.push(c); else visit(c);
    }
  };
  visit(node.slice(1));
  return out;
}

// Analyzes a paragraph. Returns:
//   ul:     [start, end) spans to underline (selbri of bridi_tail_3),
//   paints: [start, end, quoted] spans in parse-tree pre-order, so that later
//           (inner) ones override earlier (outer) ones,
//   subs:   [start, end, nesting] spans of subsentences,
//   lujvo:  [start, end] spans of lujvo,
//   error:  the parse error, if any (ul and paints are then empty).
// Offsets are into the parser input; see analyze() for the mapping back.
function walk(tree, input) {
  const ul = [], paints = [], subs = [], lujvo = [];
  const visit = (node, nesting) => {
    if (!Array.isArray(node)) return;
    if (node[0] === 'bridi_tail_3') {
      const sel = children(node).map(tag).find((t) => t && t.name === 'selbri');
      if (sel) ul.push([sel.start, sel.end]);
    }
    const t = tag(node);
    if (t && t.name === 'subsentence') subs.push([t.start, t.end, nesting++]);
    if (t && t.name === 'lujvo') lujvo.push([t.start, t.end]);
    if (t && t.name === 'sumti_6') {
      const kids = children(node).map(tag).filter(Boolean);
      const first = kids[0];
      if (first && first.name === 'ZO_clause') {
        // Just the one quoted word, not the indicators that may follow it.
        const m = /^(\s*zo[\s,]*)(\S+)/.exec(input.slice(first.start, first.end));
        const s = first.start + m[1].length;
        paints.push([s, s + m[2].length, true]);
      } else if (first && first.name === 'LU_clause') {
        const body = kids.find((k) => k.name === 'text');
        if (body) paints.push([first.end, body.end, true]);
      }
    } else if (t && t.name === 'free') {
      const first = children(node).map(tag).find(Boolean);
      if (first && first.name === 'SEI_clause') paints.push([t.start, t.end, false]);
    }
    for (const c of node) visit(c, nesting);
  };
  visit(tree, 0);
  return { ul, paints, subs, lujvo };
}

const isDialogue = (text) => text.startsWith('— ');

function analyze(text) {
  const { input, map } = preprocess(text);
  const n = text.length;
  const quoted = new Array(n).fill(false);
  const depth = new Array(n).fill(0);
  const paint = (s, e, v) => { for (let i = s; i < e; i++) quoted[i] = v; };
  // Bracket markup to insert before character i (or at the end, i = n).
  const opens = [], closes = [], notes = [];

  // Paragraphs starting with a dash are quotes as a whole.
  if (isDialogue(text)) paint(text.search(/[^—\s]/), n, true);

  // A sei clause's span, widened to the dashes around it ("—sei ... —").
  const seiSpan = (s, e) => {
    // The parser counts the pause before a following ".i" as part of the clause.
    while (e > s && /[\s.,]/.test(text[e - 1])) e--;
    let a = s; while (a > 0 && /\s/.test(text[a - 1])) a--;
    if (text[a - 1] === '—') s = a - 1;
    let b = e; while (b < n && /\s/.test(text[b])) b++;
    if (text[b] === '—') e = b + 1;
    return [s, e];
  };

  let error = null;
  try {
    const { ul, paints, subs, lujvo } = walk(camxes.parse(input), input);
    for (const [a, b] of lujvo) {
      let s = map[a], e = map[b];
      while (e > s && /[\s.,]/.test(text[e - 1])) e--;
      const word = text.slice(s, e), veljvo = lujvoNote(word);
      if (veljvo) {
        (notes[e] ||= []).push(`<span class="mn" role="note"><span class="mn-word"><i>${esc(word)}</i> = </span>${esc(veljvo)}</span>`);
      }
    }
    for (const [a, b, k] of subs) {
      let s = map[a], e = map[b];
      while (e > s && /[\s.,]/.test(text[e - 1])) e--;
      while (s < e && /\s/.test(text[s])) s++;
      if (s === e) continue;
      const [o, c] = BRACKETS[k % BRACKETS.length];
      const id = pairs++;
      (opens[s] ||= []).push([k, `<span class="br" data-pair="${id}">${o}</span>`]);
      (closes[e] ||= []).push([k, `<span class="br" data-pair="${id}">${c}</span>`]);
    }
    for (const [a, b] of ul) {
      let s = map[a], e = map[b];
      while (e > s && /[\s.,]/.test(text[e - 1])) e--;
      while (s < e && /\s/.test(text[s])) s++;
      for (let i = s; i < e; i++) depth[i]++;
    }
    for (const [a, b, v] of paints) {
      const [s, e] = v ? [map[a], map[b]] : seiSpan(map[a], map[b]);
      paint(s, e, v);
    }
  } catch (err) {
    error = String((err && err.message) || err);
    // Fallback: find quotes with regexes.
    for (const m of text.matchAll(/«lu\s+([^«»]*?)\s*(?:li'u»|$)/g)) {
      const s = m.index + m[0].indexOf(m[1]);
      paint(s, s + m[1].length, true);
    }
    for (const m of text.matchAll(/(?<![a-z'])zo\s+(\S+)/g)) {
      const s = m.index + m[0].length - m[1].length;
      paint(s, s + m[1].length, true);
    }
    for (const m of text.matchAll(/—sei\b[^—]*—?/g)) paint(m.index, m.index + m[0].length, false);
  }

  // Shading looks odd on the spaces at the edges of a quote.
  for (let i = 0; i < n; ) {
    if (!quoted[i]) { i++; continue; }
    let j = i; while (j < n && quoted[j]) j++;
    let a = i; while (a < j && /\s/.test(text[a])) quoted[a++] = false;
    let b = j; while (b > a && /\s/.test(text[b - 1])) quoted[--b] = false;
    i = j;
  }

  // At each position: inner closing brackets, then outer opening brackets.
  const ins = [];
  for (let i = 0; i <= n; i++) {
    const c = (closes[i] || []).sort((x, y) => y[0] - x[0]).map((x) => x[1]);
    const o = (opens[i] || []).sort((x, y) => x[0] - y[0]).map((x) => x[1]);
    const m = notes[i] || [];
    if (c.length || o.length || m.length) ins[i] = m.join('') + c.join('') + o.join('');
  }

  return { quoted, depth, ins, error };
}

// Subsentence brackets, by nesting depth.
// Matching brackets share a data-pair number, so the page can highlight between them.
let pairs = 0;
const BRACKETS = [['[', ']'], ['{', '}'], ['(', ')'], ['⟨', '⟩']];

// Renders text with quoted runs in <span class="q">, underline depth as
// nested <u>s inside them, and ins[i] (bracket markup) before character i.
function renderText(text, quoted, depth, ins) {
  // Wider gap before each new sentence: " .i" → marker (same length, so offsets hold),
  // emitted as two NBSPs below.
  text = text.replace(/ (?=\.i(?![a-z'.]))/g, '\u2002');
  let html = '';
  const n = text.length;
  for (let i = 0; i < n; ) {
    let j = i; while (j < n && quoted[j] === quoted[i]) j++;
    let inner = '';
    for (let k = i; k < j; ) {
      let l = k + 1; while (l < j && depth[l] === depth[k] && !ins[l]) l++;
      const d = depth[k];
      inner += (ins[k] || '') + '<u>'.repeat(d) + esc(text.slice(k, l)).replace(/\u2002/g, '&nbsp;&nbsp;') + '</u>'.repeat(d);
      k = l;
    }
    html += quoted[i] ? `<span class="q">${inner}</span>` : inner;
    i = j;
  }
  return html + (ins[n] || '');
}

// -------------------------------------------------------------- rendering --

const NUMERALS = { no: 0, pa: 1, re: 2, ci: 3, vo: 4, mu: 5, xa: 6, ze: 7, bi: 8, so: 9 };
const lojbanNumber = (s) => {
  let n = 0;
  for (let i = 0; i < s.length; i += 2) n = n * 10 + NUMERALS[s.slice(i, i + 2)];
  return n;
};

const md = read('content.md');
const blocks = md.split(/\n\s*\n/).map((b) => b.trim().replace(/\s*\n\s*/g, ' ')).filter(Boolean);

let title = '';
const body = [];
const nav = [];
let inHeader = false, inSection = false;
let parsed = 0, failed = 0;
const OPTIONS = `<div class="opts">
<!-- <label><input type="checkbox" data-class="selbri"> selbrisni linji</label> -->
<label><input type="checkbox" data-class="subs"> brisni bitmu barna</label>
</div>`;
const closeHeader = () => { if (inHeader) body.push(OPTIONS, '</header>'); inHeader = false; };
// Escapes text, turning [text](url) into links.
const inline = (s) => {
  let out = '', pos = 0;
  for (const m of s.matchAll(/\[([^\]]*)\]\(([^)\s]+)\)/g)) {
    out += esc(s.slice(pos, m.index)) + `<a href="${esc(m[2])}">${esc(m[1])}</a>`;
    pos = m.index + m[0].length;
  }
  return out + esc(s.slice(pos));
};
const closeSection = () => { if (inSection) body.push('</section>'); inSection = false; };

// French original and English translation per paragraph (see align/align.py).
const translations = JSON.parse(read('translations.json'));
let chapter = -1, para = 0, trCount = 0, chapterId = '';
// Returns [id, actions, popover] for the current paragraph: an anchor id, the
// link and translation buttons in its left margin, and the translation popover.
const icon = (name) => `<svg aria-hidden="true"><use href="#i-${name}"/></svg>`;
const paraActions = () => {
  const k = para++;
  const id = `${chapterId}-${k + 1}`;
  const num = `<span class="pn">(${chapterId}.${k + 1})</span>`;
  const link = `<button class="pl" data-id="${id}" aria-label="ponse lo judri">${icon('link')}</button>`;
  const t = (translations[chapter] || [])[k];
  if (!t) return [id, `<span class="pa">${num}${link}</span>`, ''];
  trCount++;
  const part = (lang, ps) => ps.length
    ? `<div class="tr-${lang}" lang="${lang}">${ps.map((x) => `<p>${esc(x)}</p>`).join('')}</div>`
    : '';
  const popId = `tr-${id}`;
  return [
    id,
    `<span class="pa">${num}${link}<button class="tr" popovertarget="${popId}" aria-label="fanva">${icon('tr')}</button></span>`,
    `<div class="trpop" id="${popId}" popover>${part('fr', t.fr)}${part('en', t.en)}</div>`,
  ];
};

for (const b of blocks) {
  let m;
  if ((m = /^# (.*)$/.exec(b))) {
    title = m[1];
    body.push('<header class="cover">', `<h1>${esc(title)}</h1>`);
    inHeader = true;
  } else if ((m = /^## (.*)$/.exec(b))) {
    closeHeader(); closeSection();
    const n = lojbanNumber(m[1].split(' ')[0]);
    body.push(`<section id="mo${n}" class="chapter">`, `<h2>${esc(m[1])}<span class="dg">${n}</span></h2>`);
    nav.push(`<li><a href="#mo${n}">${n} mo'o</a></li>`);
    inSection = true;
    chapter++; para = 0; chapterId = n;
  } else if ((m = /^### (.*)$/.exec(b))) {
    closeHeader(); closeSection();
    body.push(`<p class="end">${esc(m[1])}</p>`);
  } else if ((m = /^!\[(.*?)\]\((.*?)\)(?:\{width=(\d+%)\})?$/.exec(b))) {
    const style = m[3] ? ` style="width:${m[3]}"` : '';
    body.push(`<figure><img src="${esc(m[2])}" alt="${esc(m[1])}" loading="lazy"${style}></figure>`);
  } else if ((m = /^\*(.*)\*$/.exec(b))) {
    const [id, btn, pop] = inSection ? paraActions() : ['', '', ''];
    const idAttr = id ? ` id="${id}"` : '';
    body.push(`<p${idAttr} class="${inHeader ? 'byline' : 'sub'}">${btn}${inline(m[1])}</p>${pop}`);
  } else {
    const classes = isDialogue(b) ? ['dlg'] : [];
    const r = analyze(b);
    parsed++;
    let attrs = '';
    if (r.error) {
      failed++;
      classes.push('bad');
      attrs = ` title="${esc(r.error)}"`;
    }
    const cls = classes.length ? ` class="${classes.join(' ')}"` : '';
    const [id, btn, pop] = inSection ? paraActions() : ['', '', ''];
    if (id) attrs = ` id="${id}"` + attrs;
    body.push(`<p${cls}${attrs}>${btn}${renderText(b, r.quoted, r.depth, r.ins)}</p>${pop}`);
  }
}
closeHeader(); closeSection();

const html = read('template.html')
  .replaceAll('{{TITLE}}', esc(title))
  .replace('{{NAV}}', () => nav.join('\n'))
  .replace('{{BODY}}', () => body.join('\n'));
fs.writeFileSync(path.join(dir, 'index.html'), html);
console.log(`index.html: ${parsed} paragraphs, ${failed} failed to parse, ${trCount} translations`);
const trTotal = translations.reduce((n, c) => n + c.length, 0);
if (trCount !== trTotal) console.warn(`warning: translations.json has ${trTotal} paragraphs; rerun align/align.py`);
