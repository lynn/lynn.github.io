import {digits, cells, row, col, bit, candidates, single, deductions} from './logic.mjs'

const byId = id => document.getElementById(id)
const board = byId('board')
const log = byId('log')
const reason = byId('reason-field')
const chosenReason = () => reason.querySelector('input:checked')
const shortcuts = new Map([...reason.querySelectorAll('input')].map(input =>
  [input.getAttribute('aria-keyshortcuts').toLowerCase(), input]))
const techniqueNotes = {
  'Locked candidates': [60, 65, 70],   // C F B♭
  'Naked pair': [62, 69, 76],          // D A E
  'Naked triple': [65, 70, 75],        // F B♭ E♭
  'Naked quad': [55, 60, 65, 70],      // G C F B♭
  'Hidden pair': [76, 71, 64],          // E B E
  'Chain': [57, 62, 67, 72],           // A D G C
  'X-Wing': [60, 67, 65, 74],          // C G F D
  'Swordfish': [55, 62, 67, 69, 77]    // G D G A F
}
const storageKey = 'sudoku-studio-v1'
function readSave() {
  try { return JSON.parse(localStorage.getItem(storageKey)) || {} }
  catch { return {} }
}
const saved = readSave()
// Older saves used a catalog index. Their source labels preserve the clues.
let puzzle = typeof saved.puzzle === 'string' && /^[0-9]{81}$/.test(saved.puzzle)
  ? saved.puzzle
  : saved.sources?.length === 81 && saved.grid?.length === 81
    ? saved.grid.map((d, i) => saved.sources[i] === 'given' ? d : 0).join('')
    : undefined
let givens, grid, removed, sources
let history = []
let selected = 0
let selection = new Set([selected])
let started = false
let busy = false
let timer, popping, hint, warning
let audio
let dragPointer
let lastHintKey, nextDeduction
const hintPriority = ['Naked pair', 'X-Wing', 'Chain']
const moveLabel = move => move.pattern ? move.name + ' (' + move.pattern + ')' : move.name

function configure(clues) {
  puzzle = clues
  givens = [...clues].map(Number)
}

function logMove(message) {
  const entry = document.createElement('li')
  entry.textContent = message
  log.append(entry)
  log.scrollTop = log.scrollHeight
}

function save() {
  try {
    localStorage.setItem(storageKey, JSON.stringify({
      puzzle, grid, removed, sources, history, started
    }))
  } catch {
    // Play remains available when browser storage is disabled.
  }
}

function unlockAudio() {
  if (!window.AudioContext) return
  audio ||= new AudioContext()
  if (audio.state === 'suspended') audio.resume().catch(() => {})
}

const popPitch = {row: 700, column: 910, box: 1175}

function playSingle(unit) {
  if (audio?.state !== 'running') return
  const now = audio.currentTime
  const oscillator = audio.createOscillator()
  const gain = audio.createGain()
  const pitch = popPitch[unit]
  const duration = pitch ? .04 : .012
  oscillator.type = pitch ? 'sine' : 'triangle'
  oscillator.frequency.setValueAtTime(pitch ?? 2300, now)
  if (pitch) oscillator.frequency.exponentialRampToValueAtTime(pitch / 3, now + .025)
  gain.gain.setValueAtTime(.001, now)
  gain.gain.exponentialRampToValueAtTime(pitch ? .2 : .095, now + .002)
  gain.gain.exponentialRampToValueAtTime(.001, now + duration - .002)
  oscillator.connect(gain).connect(audio.destination)
  oscillator.start(now)
  oscillator.stop(now + duration)
}

function playNotes(notes, spacing = .038, duration = .23, volume = .055, shimmer = false) {
  if (!audio) return
  const play = () => {
    if (audio.state !== 'running') return
    const now = audio.currentTime
    const echo = shimmer ? audio.createDelay() : undefined
    if (echo) {
      const wet = audio.createGain()
      echo.delayTime.value = .085
      wet.gain.value = .18
      echo.connect(wet).connect(audio.destination)
      const tail = duration + (notes.length - 1) * spacing + echo.delayTime.value
      setTimeout(() => { echo.disconnect(); wet.disconnect() }, (tail + .05) * 1000)
    }
    for (const [index, note] of notes.entries()) {
      const start = now + index * spacing
      const frequency = 440 * 2 ** ((note - 69) / 12)
      function voice(hz, level, release, detune = 0) {
        const oscillator = audio.createOscillator()
        const gain = audio.createGain()
        oscillator.frequency.value = hz
        if (detune) oscillator.detune.value = detune
        gain.gain.setValueAtTime(.001, start)
        gain.gain.exponentialRampToValueAtTime(level, start + .009)
        gain.gain.exponentialRampToValueAtTime(.001, start + release)
        oscillator.connect(gain).connect(audio.destination)
        if (echo) gain.connect(echo)
        oscillator.start(start)
        oscillator.stop(start + release)
      }
      voice(frequency, volume, duration)
      if (shimmer) voice(frequency * 2, volume * .22, duration * .7, 6)
    }
  }
  if (audio.state === 'running') play()
  else audio.resume().then(play).catch(() => {})
}

const tiles = cells.map(i => {
  const button = document.createElement('button')
  button.type = 'button'
  button.style.setProperty('--victory-delay', (row(i) + col(i)) * 35 + 'ms')
  button.addEventListener('click', event => {
    if (event.detail > 0) return
    unlockAudio()
    select(i, event.shiftKey)
  })
  board.append(button)
  return button
})

board.addEventListener('pointerdown', event => {
  if (event.button !== 0 || dragPointer !== undefined) return
  const i = tiles.indexOf(event.target.closest('.cell'))
  if (i === -1) return
  event.preventDefault()
  dragPointer = event.pointerId
  board.setPointerCapture(dragPointer)
  select(i, event.shiftKey)
  tiles[selected].focus({preventScroll: true})
})
board.addEventListener('pointermove', event => {
  if (event.pointerId !== dragPointer) return
  const i = tiles.indexOf(document.elementFromPoint(event.clientX, event.clientY)?.closest('.cell'))
  if (i === -1 || selection.has(i)) return
  select(i, true)
  tiles[selected].focus({preventScroll: true})
})
board.addEventListener('lostpointercapture', event => {
  if (event.pointerId === dragPointer) dragPointer = undefined
})
const pad = digits.map(d => {
  const button = document.createElement('button')
  button.textContent = d
  button.addEventListener('click', () => enter(d))
  byId('pad').append(button)
  return button
})

function findNextDeduction(masks) {
  const key = masks.join(',')
  if (key !== lastHintKey) {
    lastHintKey = key
    nextDeduction = undefined
    let bestRank = Infinity
    for (const move of deductions(masks)) {
      const priority = hintPriority.indexOf(move.name)
      const rank = move.name === 'Hidden pair' ? hintPriority.length + 1 :
        priority === -1 ? hintPriority.length : priority
      if (rank >= bestRank) continue
      nextDeduction = move
      bestRank = rank
      if (rank === 0) break
    }
  }
  return nextDeduction
}

function render() {
  const masks = candidates(grid, removed)
  const complete = grid.every(Boolean)
  const matching = grid[selected]
  const counts = Array(10).fill(0)
  for (const value of grid) counts[value]++
  const available = [...selection].reduce((mask, i) => mask | masks[i], 0)
  for (const i of cells) {
    const tile = tiles[i]
    const value = grid[i]
    const cuts = hint?.cuts.find(([j]) => j === i)?.[1] || 0
    tile.className = ['cell', sources[i],
      matching && matching === value && 'same', selection.has(i) && 'selected', hint?.anchors.includes(i) && 'anchor',
      cuts && 'target',
      i === popping && 'popping'].filter(Boolean).join(' ')
    tile.setAttribute('aria-pressed', String(selection.has(i)))
    tile.tabIndex = i === selected ? 0 : -1
    const options = digits.filter(d => masks[i] & bit(d))
    tile.setAttribute('aria-label', 'Row ' + (row(i) + 1) + ', column ' + (col(i) + 1) +
      (value ? ', ' + value + ', ' + sources[i] : ', candidates ' + options.join(', ')))
    const content = value ? '<span class="value' + (counts[value] === 9 ? ' complete-digit' : '') + '">' + value + '</span>' :
      '<span class="notes">' + digits.map(d => '<span class="note' + (cuts & bit(d) ? ' cut' : '') +
        (d === matching && (masks[i] & bit(d)) ? ' match' : '') +
        '">' + (masks[i] & bit(d) ? d : '') + '</span>').join('') + '</span>'
    if (tile.innerHTML !== content) tile.innerHTML = content
  }
  byId('message').textContent = warning || (complete ? 'Solved!' : hint ? 'Try ' + moveLabel(hint) + '.' : '')
  byId('start').hidden = started
  byId('undo').disabled = !history.length
  byId('hint').disabled = !started || busy || complete
  reason.disabled = !started || busy || complete
  for (const [index, button] of pad.entries()) {
    button.disabled = !started || busy || !(available & bit(index + 1))
  }
}

function settle(opening = false) {
  clearTimeout(timer)
  const work = [...grid]
  const singles = []
  for (let next = single(candidates(work, removed)); next; next = single(candidates(work, removed))) {
    singles.push(next)
    work[next.i] = next.d
  }
  const speed = 95
  const openingDelay = Math.max(24, speed / Math.max(1, Math.sqrt(singles.length / 3)))
  const delayFor = index => {
    if (singles.length === 1) return speed
    const progress = index / (singles.length - 1)
    return openingDelay + (speed * 1.45 - openingDelay) * progress ** 3
  }
  busy = singles.length > 0
  function finish() {
    busy = false
    popping = undefined
    if (singles.length) {
      logMove('Automated ' + singles.length + (singles.length === 1 ? ' single.' : ' singles.'))
    }
    render()
    if (grid.every(Boolean)) {
      board.classList.add('victory')
      playNotes([55, 60, 65, 70, 74, 79], .09, .52)
    } else if (opening) {
      playNotes([67, 72, 77], .045, .16, .038)
    }
  }
  function placeSingle(index) {
    const next = singles[index]
    grid[next.i] = next.d
    sources[next.i] = 'auto'
    popping = next.i
    playSingle(next.unit)
    save()
    if (index === singles.length - 1 && grid.every(Boolean)) {
      finish()
      return
    }
    render()
    timer = setTimeout(() => {
      if (index + 1 < singles.length) placeSingle(index + 1)
      else finish()
    }, delayFor(Math.min(index + 1, singles.length - 1)))
  }
  if (busy) {
    render()
    timer = setTimeout(() => placeSingle(0), delayFor(0))
  } else {
    finish()
  }
}

function reset(clues, start = true) {
  clearTimeout(timer)
  log.replaceChildren()
  board.classList.remove('victory')
  configure(clues)
  grid = [...givens]
  removed = cells.map(() => 0)
  sources = givens.map(d => d ? 'given' : '')
  history = []
  selected = grid.indexOf(0)
  selection = new Set([selected])
  const choice = chosenReason()
  if (choice) choice.checked = false
  started = start
  busy = false
  popping = hint = warning = undefined
  save()
  if (start) settle(true)
  else render()
}

function select(i, extend = false) {
  if (!extend) {
    selection = new Set([i])
    selected = i
  } else if (!selection.has(i)) {
    selection.add(i)
    selected = i
  } else if (selection.size > 1) {
    selection.delete(i)
    if (i === selected) selected = [...selection].at(-1)
  }
  warning = undefined
  render()
}

function enter(d) {
  unlockAudio()
  if (!started || busy || grid.every(Boolean)) return
  const masks = candidates(grid, removed)
  const digitMask = bit(d)
  const targets = [...selection].filter(i => masks[i] & digitMask)
  const method = chosenReason()?.value
  const labels = new Map()
  if (method && targets.length) {
    for (const move of deductions(masks)) {
      if (move.name !== method) continue
      for (const [i, cut] of move.cuts) {
        if ((cut & digitMask) && targets.includes(i) && !labels.has(i)) labels.set(i, moveLabel(move))
      }
      if (labels.size === targets.length) break
    }
  }
  const invalid = targets.find(i => !labels.has(i))
  if (!targets.length || !method || invalid !== undefined) {
    warning = !targets.length ? 'No selected cell has ' + d + ' as a candidate.' :
      !method ? 'Choose a reason before removing a candidate.' :
      method + ' cannot eliminate ' + d + ' from row ' + (row(invalid) + 1) + ', column ' + (col(invalid) + 1) + '.'
    playNotes([55, 49], .055, .15)
    render()
    return
  }
  history.push({grid: [...grid], removed: [...removed], sources: [...sources], selected, selection: [...selection]})
  for (const i of targets) removed[i] |= digitMask
  for (const label of new Set(labels.values())) {
    const coordinates = targets.filter(i => labels.get(i) === label)
      .map(i => 'R' + (row(i) + 1) + 'C' + (col(i) + 1)).join(', ')
    logMove('Eliminated ' + d + ' from ' + coordinates + ' using ' + label + '.')
  }
  const shimmer = method !== 'Locked candidates' && method !== 'Naked pair'
  playNotes(techniqueNotes[method], .038, .23, .055, shimmer)
  popping = hint = warning = undefined
  save()
  settle()
}

function undo() {
  const previous = history.pop()
  if (!previous) return
  unlockAudio()
  playNotes([74, 67], .045, .12, .034)
  clearTimeout(timer)
  board.classList.remove('victory')
  ;({grid, removed, sources, selected} = previous)
  selection = new Set(previous.selection ?? [selected])
  logMove('Undid last elimination.')
  busy = false
  popping = hint = warning = undefined
  save()
  render()
}

function requestPuzzle(start) {
  const button = byId('new')
  button.disabled = true
  button.textContent = 'Generating…'
  const layout = document.querySelector('.layout')
  layout.inert = true
  const worker = new Worker(new URL('./generator-worker.mjs', import.meta.url), {type: 'module'})
  function finish() {
    worker.terminate()
    button.disabled = false
    button.textContent = 'New puzzle'
    layout.inert = !puzzle
  }
  worker.onmessage = ({data}) => {
    reset(data, start)
    finish()
  }
  worker.onerror = () => {
    finish()
    byId('message').textContent = 'Generation failed. Select New puzzle to retry.'
  }
  worker.postMessage(null)
}
byId('new').onclick = () => { unlockAudio(); requestPuzzle(true) }
byId('restart').onclick = () => { unlockAudio(); reset(puzzle) }
byId('start').onclick = () => {
  unlockAudio()
  started = true
  save()
  settle(true)
}
byId('undo').onclick = undo
reason.onchange = () => {
  unlockAudio()
  playNotes([81], 0, .055, .026)
  warning = undefined
  render()
}
byId('hint').onclick = () => {
  hint = findNextDeduction(candidates(grid, removed))
  warning = hint ? undefined : 'No supported deduction found.'
  render()
}
document.addEventListener('pointerdown', unlockAudio)
document.addEventListener('keydown', event => {
  if (!grid || byId('new').disabled) return
  const shortcut = shortcuts.get(event.key.toLowerCase())
  if (shortcut && !reason.disabled && !event.ctrlKey && !event.metaKey && !event.altKey &&
      !event.target.matches('select, input:not([type="radio"]), textarea, [contenteditable]')) {
    event.preventDefault()
    shortcut.click()
    return
  }
  if (event.target.matches('select, input')) return
  const offset = {ArrowUp: -9, ArrowDown: 9, ArrowLeft: -1, ArrowRight: 1}[event.key]
  if (offset && board.contains(document.activeElement)) {
    event.preventDefault()
    select((selected + offset + 81) % 81)
    tiles[selected].focus()
  }
  if (/^[1-9]$/.test(event.key) && !event.ctrlKey && !event.metaKey && !event.altKey) enter(Number(event.key))
  if ((event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'z' && history.length) {
    event.preventDefault()
    undo()
  }
})

if (puzzle && saved.grid?.length === 81 && saved.removed?.length === 81 && saved.sources?.length === 81 && saved.started) {
  configure(puzzle)
  ;({grid, removed, sources} = saved)
  history = Array.isArray(saved.history) ? saved.history : []
  started = true
  selected = Math.max(0, grid.indexOf(0))
  selection = new Set([selected])
  if (grid.every(Boolean)) render()
  else settle()
} else if (puzzle) {
  reset(puzzle, false)
} else {
  requestPuzzle(false)
}
