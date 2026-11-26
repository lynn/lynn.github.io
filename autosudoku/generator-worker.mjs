import {generatePuzzle} from './puzzles.mjs'

self.onmessage = ({data}) => self.postMessage(generatePuzzle(data))
