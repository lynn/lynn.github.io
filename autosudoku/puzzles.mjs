import {cells, shuffle, search, grade, candidates, single, deductions} from './logic.mjs'

// Check every possible first deduction after the opening singles. A whole
// pattern counts as one move even when it removes several candidates.
function oneMoveSolves(givens, mode) {
  const grid = [...givens]
  function settle(board, removed) {
    let masks = candidates(board, removed)
    let next
    while ((next = single(masks))) {
      board[next.i] = next.d
      masks = candidates(board, removed)
    }
    return masks
  }
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

export function generatePuzzle(mode = 'normal') {
  for (;;) {
    const [solution] = search(Array(81).fill(0), 1, true)
    const grid = [...solution]
    for (const i of shuffle(cells)) {
      grid[i] = 0
      if (search(grid).length !== 1) grid[i] = solution[i]
    }
    // With no Normal move after the forced singles, every route must use Hard.
    if (mode === 'hard') {
      const normal = grade(grid)
      if (normal.solution || normal.methods.length) continue
    }
    const result = grade(grid, false, mode)
    if (result.methods.length >= 2 && result.solution?.every((d, i) => d === solution[i])
        && !oneMoveSolves(grid, mode)) return grid.join('')
  }
}
