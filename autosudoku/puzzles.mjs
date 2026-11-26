import {bit, digits, cells, shuffle, search, grade, hardTechniques, candidates, settleSingles, deductions} from './logic.mjs'

function settle(board, removed) {
  return settleSingles(board, candidates(board, removed))
}

// Check every possible first deduction after the opening singles. A whole
// pattern counts as one move even when it removes several candidates.
function oneMoveSolves(givens, mode) {
  const grid = [...givens]
  const masks = settle(grid, [])
  for (const move of deductions(masks, mode)) {
    const removed = []
    for (const [i, mask] of move.cuts) removed[i] = mask
    const branch = [...grid]
    settle(branch, removed)
    if (branch.every(Boolean)) return true
  }
  return false
}

// Certify that every playable Normal-only route stalls, including partial
// eliminations and multi-cell selections. Skip expensive puzzles if the
// search budget runs out before we can certify them.
function requiresHard(givens) {
  const seen = new Set()
  function stalled(grid, removed) {
    const masks = settle(grid, removed)
    if (grid.every(Boolean)) return false
    const key = masks.join(',')
    if (seen.has(key)) return true
    if (seen.size >= 128) return false
    seen.add(key)

    const byTechnique = new Map()
    for (const move of deductions(masks, 'normal')) {
      if (!byTechnique.has(move.name)) byTechnique.set(move.name, new Map())
      const cuts = byTechnique.get(move.name)
      for (const [i, mask] of move.cuts) cuts.set(i, (cuts.get(i) ?? 0) | mask)
    }
    for (const cuts of byTechnique.values()) {
      for (const d of digits) {
        const b = bit(d)
        const targets = [...cuts.keys()].filter(i => cuts.get(i) & b)
        function selectionsStall(start, marks) {
          for (let j = start; j < targets.length; j++) {
            const next = [...marks]
            next[targets[j]] |= b
            if (!stalled([...grid], next) || !selectionsStall(j + 1, next)) return false
          }
          return true
        }
        if (!selectionsStall(0, removed)) return false
      }
    }
    return true
  }
  return stalled([...givens], cells.map(() => 0))
}

export function generatePuzzle(mode = 'normal') {
  for (;;) {
    const [solution] = search(Array(81).fill(0), 1, true)
    const grid = [...solution]
    for (const i of shuffle(cells)) {
      grid[i] = 0
      // The previous grid was unique. Any second solution must change this clue.
      if (search(grid, 1, false, i, solution[i]).length) grid[i] = solution[i]
    }
    const result = grade(grid, false, mode)
    if (result.methods.length >= 2 && result.solution?.every((d, i) => d === solution[i])
        && (mode !== 'hard' || result.methods.some(name => hardTechniques.includes(name)))
        && !oneMoveSolves(grid, mode) && (mode !== 'hard' || requiresHard(grid))) return grid.join('')
  }
}
