export const digits = [1, 2, 3, 4, 5, 6, 7, 8, 9]
export const cells = Array.from({length: 81}, (_, i) => i)
export const row = i => Math.floor(i / 9)
export const col = i => i % 9
export const box = i => Math.floor(row(i) / 3) * 3 + Math.floor(col(i) / 3)
export const bit = d => 1 << (d - 1)
const full = 511
const bits = digits.map(bit)
const count = Array.from({length: 512}, (_, m) => bits.filter(b => m & b).length)
const lines = [row, col, box].flatMap(axis => digits.map(d => cells.filter(i => axis(i) === d - 1)))
const units = cells.map(i => [row(i), col(i) + 9, box(i) + 18])
const unitMasks = units.map(([r, c, b]) => (1 << r) | (1 << c) | (1 << b))
const boxBits = cells.map(i => 1 << box(i))
// Only row/box and column/box intersections can contain a locked group.
const crossings = lines.map(line => lines.filter(other =>
  other !== line && other.filter(i => line.includes(i)).length === 3
).map(other => ({unitBit: 1 << lines.indexOf(other), outside: other.filter(i => !line.includes(i))})))
const rowBits = cells.map(i => 1 << row(i))
const colBits = cells.map(i => 1 << col(i))
const fishAxes = [
  {baseBits: rowBits, coverBits: colBits, offset: 0},
  {baseBits: colBits, coverBits: rowBits, offset: 9}
]
const visible = cells.map(a => cells.map(b => a !== b && (row(a) === row(b) || col(a) === col(b) || box(a) === box(b))))
export const sees = (a, b) => visible[a][b]
const peers = cells.map(i => cells.filter(j => sees(i, j)))

export function candidates(grid, removed = []) {
  const used = new Uint16Array(27)
  for (const i of cells) {
    if (grid[i]) for (const u of units[i]) used[u] |= bit(grid[i])
  }
  return cells.map(i => {
    const [r, c, b] = units[i]
    return grid[i] ? 0 : full & ~(used[r] | used[c] | used[b] | removed[i])
  })
}

export function single(masks) {
  const i = masks.findIndex(m => count[m] === 1)
  if (i >= 0) return {i, d: Math.log2(masks[i]) + 1, name: 'Naked single'}
  for (const [index, line] of lines.entries()) {
    let seen = 0, repeated = 0
    for (const j of line) {
      repeated |= seen & masks[j]
      seen |= masks[j]
    }
    const unique = seen & ~repeated
    if (unique) {
      const b = unique & -unique
      return {i: line.find(j => masks[j] & b), d: Math.log2(b) + 1,
        name: 'Hidden single', unit: ['row', 'column', 'box'][Math.floor(index / 9)]}
    }
  }
}

export function settleSingles(grid, masks) {
  for (let next; (next = single(masks));) {
    grid[next.i] = next.d
    masks[next.i] = 0
    const b = bit(next.d)
    for (const j of peers[next.i]) masks[j] &= ~b
  }
  return masks
}

function* combinations(items, n) {
  // All callers request pairs, triples or quads. Yield only complete groups.
  for (let a = 0; a <= items.length - n; a++) {
    for (let b = a + 1; b <= items.length - n + 1; b++) {
      if (n === 2) {
        yield [items[a], items[b]]
        continue
      }
      for (let c = b + 1; c <= items.length - n + 2; c++) {
        if (n === 3) yield [items[a], items[b], items[c]]
        else for (let d = c + 1; d < items.length; d++) yield [items[a], items[b], items[c], items[d]]
      }
    }
  }
}

export function* deductions(masks, mode = 'normal') {
  const active = cells.filter(i => masks[i])
  function step(name, anchors, targets, mask, anchorMask = mask) {
    const cuts = targets.filter(i => masks[i] & mask).map(i => [i, masks[i] & mask])
    if (cuts.length) return {name, anchors, anchorMask, cuts}
  }
  const locationsByDigit = bits.map(b => lines.map(line => line.filter(i => masks[i] & b)))
  for (const [digitIndex, b] of bits.entries()) {
    for (const [index, anchors] of locationsByDigit[digitIndex].entries()) {
      if (anchors.length < 2 || anchors.length > 3) continue
      let sharedUnits = unitMasks[anchors[0]] & unitMasks[anchors[1]]
      if (anchors.length === 3) sharedUnits &= unitMasks[anchors[2]]
      for (const crossing of crossings[index]) {
        if (!(sharedUnits & crossing.unitBit)) continue
        const result = step('Locked candidates', anchors, crossing.outside, b)
        if (result) yield result
      }
    }
  }
  for (const [n, name] of [[2, 'Naked pair'], [3, 'Naked triple'], [4, 'Naked quad']]) {
    for (const [index, line] of lines.entries()) {
      for (const anchors of combinations(line.filter(i => masks[i] && count[masks[i]] <= n), n)) {
        const mask = anchors.reduce((m, i) => m | masks[i], 0)
        if (count[mask] !== n) continue
        const result = step(name, anchors, line.filter(i => !anchors.includes(i)), mask)
        if (result) yield result
      }
      if (n !== 2) continue
      const pairs = bits.map((b, d) => ({b, cells: locationsByDigit[d][index]})).filter(p => p.cells.length === 2)
      for (const [a, b] of combinations(pairs, 2)) {
        if (a.cells[0] !== b.cells[0] || a.cells[1] !== b.cells[1]) continue
        const result = step('Hidden pair', a.cells, a.cells, full & ~(a.b | b.b), a.b | b.b)
        if (result) yield result
      }
    }
  }
  for (const [digitIndex, b] of bits.entries()) {
    const locations = locationsByDigit[digitIndex]
    for (const {baseBits, coverBits, offset} of fishAxes) {
      for (const n of (mode === 'hard' ? [2, 3] : [2])) {
        const groups = locations.slice(offset, offset + 9).filter(group => group.length >= 2 && group.length <= n)
        for (const selection of combinations(groups, n)) {
          let cover = 0, bases = 0
          for (const group of selection) {
            bases |= baseBits[group[0]]
            for (const i of group) cover |= coverBits[i]
          }
          if (count[cover] !== n) continue
          const targets = active.filter(i => (cover & coverBits[i]) && !(bases & baseBits[i]))
          const result = step(n === 2 ? 'X-Wing' : 'Swordfish', selection.flat(), targets, b)
          if (result) yield result
        }
      }
    }
    const links = locations.flatMap((points, i) => points.length === 2 ? [{points, axis: Math.floor(i / 9)}] : [])
    for (const [a, c] of combinations(links, 2)) {
      if (a.axis === 2 && c.axis === 2) continue
      const pattern = c.axis === 2 ? 'Crane' : a.axis === c.axis ? 'Skyscraper' : 'Two-String Kite'
      if (a.points.some(i => c.points.includes(i))) continue
      for (let j = 0; j < 2; j++) {
        const tip = a.points[j], joint = a.points[1 - j]
        for (let k = 0; k < 2; k++) {
          const otherTip = c.points[k], otherJoint = c.points[1 - k]
          if (!sees(joint, otherJoint)) continue
          const anchors = [tip, joint, otherJoint, otherTip]
          if (a.axis === 0 && c.axis === 1 && box(joint) !== box(otherJoint)) continue
          const result = step('Chain', anchors, active.filter(i => !anchors.includes(i) && sees(i, tip) && sees(i, otherTip)), b)
          if (result) yield {...result, pattern}
        }
      }
    }
  }
  if (mode !== 'hard') return
  const pairs = active.filter(i => count[masks[i]] === 2)
  for (const pivot of pairs) {
    const wings = pairs.filter(i => sees(i, pivot) && count[masks[i] & masks[pivot]] === 1)
    for (const [a, b] of combinations(wings, 2)) {
      const shared = masks[a] & masks[b] & ~masks[pivot]
      if (!shared || masks[a] === masks[b]) continue
      const result = step('Y-Wing', [pivot, a, b], peers[a].filter(i => sees(i, b)), shared, masks[pivot] | shared)
      if (result) yield result
    }
  }
  for (const [a, b] of combinations(pairs, 2)) {
    if (masks[a] !== masks[b] || sees(a, b)) continue
    for (const link of bits.filter(d => masks[a] & d)) {
      for (const ends of locationsByDigit[Math.log2(link)]) {
        if (ends.length !== 2 || ends.includes(a) || ends.includes(b)) continue
        const [x, y] = ends
        if (!(sees(a, x) && sees(b, y) || sees(a, y) && sees(b, x))) continue
        const result = step('W-Wing', [a, b, x, y], peers[a].filter(i => sees(i, b)), masks[a] ^ link)
        if (result) yield {...result, anchorMasks: {[a]: masks[a], [b]: masks[b], [x]: link, [y]: link}}
      }
    }
  }
  // Type 1: three exact pairs force the fourth corner to use another digit
  // in a unique puzzle, or the two digits could swap throughout the rectangle.
  for (const corner of pairs) {
    const pair = masks[corner]
    for (const a of lines[row(corner)]) {
      if (a === corner || masks[a] !== pair) continue
      for (const b of lines[9 + col(corner)]) {
        if (b === corner || masks[b] !== pair) continue
        const roof = row(b) * 9 + col(a)
        const boxes = boxBits[corner] | boxBits[a] | boxBits[b] | boxBits[roof]
        if (count[boxes] !== 2 ||
            masks[roof] === pair || (masks[roof] & pair) !== pair) continue
        yield step('Unique Rectangle', [corner, a, b], [roof], pair)
      }
    }
  }
}

export const hardTechniques = ['Swordfish', 'Y-Wing', 'W-Wing', 'Unique Rectangle']
export const advanced = ['Chain', 'X-Wing', ...hardTechniques]

export function canEliminate(masks, method, cell, digitMask, mode = 'normal') {
  for (const move of deductions(masks, mode)) {
    if (move.name === method && move.cuts.some(([i, cut]) => i === cell && (cut & digitMask))) return true
  }
  return false
}

export function grade(givens, basicOnly = false, mode = 'normal') {
  const grid = [...givens]
  const masks = candidates(grid)
  const methods = []
  while (grid.includes(0)) {
    settleSingles(grid, masks)
    if (!grid.includes(0)) break
    const step = deductions(masks, mode).next().value
    if (!step || (basicOnly && advanced.includes(step.name))) return {methods}
    methods.push(step.name)
    for (const [i, mask] of step.cuts) masks[i] &= ~mask
  }
  return {solution: grid, methods}
}

export function shuffle(values) {
  const list = [...values]
  for (let i = list.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1))
    ;[list[i], list[j]] = [list[j], list[i]]
  }
  return list
}

// Count up to the supplied limit using the cell with the fewest legal moves.
export function search(grid, limit = 2, random = false, excludedCell = -1, excludedDigit = 0) {
  const work = [...grid]
  const empty = cells.filter(i => !grid[i])
  const used = new Uint16Array(27)
  function toggle(i, b) {
    for (const u of units[i]) used[u] ^= b
  }
  for (const i of cells) {
    if (work[i]) toggle(i, bit(work[i]))
  }
  const solutions = []
  function visit() {
    let best = -1
    let options = 0
    let size = 10
    for (let index = 0; index < empty.length; index++) {
      const i = empty[index]
      if (work[i]) continue
      const unit = units[i]
      let mask = full & ~(used[unit[0]] | used[unit[1]] | used[unit[2]])
      if (i === excludedCell) mask &= ~bit(excludedDigit)
      if (!mask) return
      if (count[mask] >= size) continue
      best = i
      options = mask
      size = count[mask]
      if (size === 1) break
    }
    if (best < 0) {
      solutions.push([...work])
      return
    }
    for (const d of random ? shuffle(digits) : digits) {
      const b = bit(d)
      if (!(b & options)) continue
      work[best] = d
      toggle(best, b)
      visit()
      toggle(best, b)
      work[best] = 0
      if (solutions.length >= limit) break
    }
  }
  visit()
  return solutions
}
