import {generatePuzzle} from './puzzles.mjs'

self.onmessage = () => self.postMessage(generatePuzzle())
