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
const axes = [row, col]
export const sees = (a, b) => a !== b && (row(a) === row(b) || col(a) === col(b) || box(a) === box(b))
const peers = cells.map(i => cells.filter(j => sees(i, j)))

export function candidates(grid, removed = []) {
  return cells.map(i => grid[i] ? 0 : peers[i].reduce(
    (mask, j) => grid[j] ? mask & ~bit(grid[j]) : mask, full & ~(removed[i] || 0)
  ))
}

export function single(masks) {
  const i = masks.findIndex(m => count[m] === 1)
  if (i >= 0) return {i, d: Math.log2(masks[i]) + 1, name: 'Naked single'}
  for (const [index, line] of lines.entries()) {
    for (const d of digits) {
      const options = line.filter(j => masks[j] & bit(d))
      if (options.length === 1) {
        return {i: options[0], d, name: 'Hidden single', unit: ['row', 'column', 'box'][Math.floor(index / 9)]}
      }
    }
  }
}

function* combinations(items, n, start = 0, chosen = []) {
  if (!n) {
    yield chosen
    return
  }
  for (let i = start; i <= items.length - n; i++) {
    yield* combinations(items, n - 1, i + 1, [...chosen, items[i]])
  }
}

export function* deductions(masks) {
  const active = cells.filter(i => masks[i])
  function step(name, anchors, targets, mask, anchorMask = mask) {
    const cuts = targets.filter(i => masks[i] & mask).map(i => [i, masks[i] & mask])
    if (cuts.length) return {name, anchors, anchorMask, cuts}
  }
  for (const b of bits) {
    for (const line of lines) {
      const anchors = line.filter(i => masks[i] & b)
      if (anchors.length < 2) continue
      for (const crossing of lines) {
        if (crossing === line || !anchors.every(i => crossing.includes(i))) continue
        const result = step('Locked candidates', anchors, crossing.filter(i => !line.includes(i)), b)
        if (result) yield result
      }
    }
  }
  for (const [n, name] of [[2, 'Naked pair'], [3, 'Naked triple'], [4, 'Naked quad']]) {
    for (const line of lines) {
      for (const anchors of combinations(line.filter(i => masks[i] && count[masks[i]] <= n), n)) {
        const mask = anchors.reduce((m, i) => m | masks[i], 0)
        if (count[mask] !== n) continue
        const result = step(name, anchors, line.filter(i => !anchors.includes(i)), mask)
        if (result) yield result
      }
      if (n !== 2) continue
      const pairs = bits.map(b => ({b, cells: line.filter(i => masks[i] & b)})).filter(p => p.cells.length === 2)
      for (const [a, b] of combinations(pairs, 2)) {
        if (a.cells[0] !== b.cells[0] || a.cells[1] !== b.cells[1]) continue
        const result = step('Hidden pair', a.cells, a.cells, full & ~(a.b | b.b), a.b | b.b)
        if (result) yield result
      }
    }
  }
  for (const b of bits) {
    const locations = lines.map(line => line.filter(i => masks[i] & b))
    for (const [axis, across] of [axes, [...axes].reverse()]) {
      const offset = axis === row ? 0 : 9
      for (const n of [2, 3]) {
        const groups = locations.slice(offset, offset + 9).filter(group => group.length >= 2 && group.length <= n)
        for (const selection of combinations(groups, n)) {
          const anchors = selection.flat()
          const cover = new Set(anchors.map(across))
          if (cover.size !== n) continue
          const bases = new Set(anchors.map(axis))
          const result = step(n === 2 ? 'X-Wing' : 'Swordfish', anchors, active.filter(i => cover.has(across(i)) && !bases.has(axis(i))), b)
          if (result) yield result
        }
      }
    }
    const links = locations.flatMap((points, i) => points.length === 2 ? [{points, axis: Math.floor(i / 9)}] : [])
    for (const [a, c] of combinations(links, 2)) {
      if (a.axis === 2 && c.axis === 2) continue
      const pattern = c.axis === 2 ? 'Crane' : a.axis === c.axis ? 'Skyscraper' : 'Two-String Kite'
      for (const [tip, joint] of [a.points, [...a.points].reverse()]) {
        for (const [otherTip, otherJoint] of [c.points, [...c.points].reverse()]) {
          const anchors = [tip, joint, otherJoint, otherTip]
          if (new Set(anchors).size !== 4 || !sees(joint, otherJoint)) continue
          if (a.axis === 0 && c.axis === 1 && box(joint) !== box(otherJoint)) continue
          const result = step('Chain', anchors, active.filter(i => !anchors.includes(i) && sees(i, tip) && sees(i, otherTip)), b)
          if (result) yield {...result, pattern}
        }
      }
    }
  }
}

export const advanced = ['Chain', 'X-Wing', 'Swordfish']

export function canEliminate(masks, method, cell, digitMask) {
  for (const move of deductions(masks)) {
    if (move.name === method && move.cuts.some(([i, cut]) => i === cell && (cut & digitMask))) return true
  }
  return false
}

export function grade(givens, basicOnly = false) {
  const grid = [...givens]
  const removed = cells.map(() => 0)
  const methods = []
  while (grid.includes(0)) {
    const masks = candidates(grid, removed)
    const next = single(masks)
    if (next) {
      grid[next.i] = next.d
      continue
    }
    const step = deductions(masks).next().value
    if (!step || (basicOnly && advanced.includes(step.name))) return {methods}
    methods.push(step.name)
    for (const [i, mask] of step.cuts) removed[i] |= mask
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
export function search(grid, limit = 2, random = false) {
  const work = [...grid]
  const used = new Uint16Array(27)
  const units = cells.map(i => [row(i), col(i) + 9, box(i) + 18])
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
    for (const i of cells) {
      if (work[i]) continue
      const [a, b, c] = units[i]
      const mask = full & ~(used[a] | used[b] | used[c])
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
