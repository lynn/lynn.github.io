/* tslint:disable */
/* eslint-disable */

/**
 * Generate gismu candidates from source words.
 *
 * * `words_str` – source words (one per language weight), separated by commas
 *   and/or whitespace.
 * * `weights_str` – comma-separated weights, or a preset name (e.g. `"1995"`).
 * * `shapes_str` – comma-separated candidate shapes (e.g. `"ccvcv,cvccv"`).
 * * `all_letters` – use the full consonant/vowel inventory instead of only
 *   the letters present in the input words.
 * * `deduplicate` – flag candidates that are too similar to existing gismu and
 *   pick a winner that avoids such clashes.
 * * `require_rafsi` – drop any candidate whose possible short rafsi are all
 *   already assigned to official words (leaving it no free short rafsi).
 * * `limit` – maximum number of scored candidates to return.
 *
 * Returns a JSON string on success, or rejects with an error message.
 */
export function generate_gismu(words_str: string, weights_str: string, shapes_str: string, all_letters: boolean, deduplicate: boolean, require_rafsi: boolean, limit: number): string;

export type InitInput = RequestInfo | URL | Response | BufferSource | WebAssembly.Module;

export interface InitOutput {
    readonly memory: WebAssembly.Memory;
    readonly generate_gismu: (a: number, b: number, c: number, d: number, e: number, f: number, g: number, h: number, i: number, j: number) => [number, number, number, number];
    readonly __wbindgen_externrefs: WebAssembly.Table;
    readonly __wbindgen_malloc: (a: number, b: number) => number;
    readonly __wbindgen_realloc: (a: number, b: number, c: number, d: number) => number;
    readonly __externref_table_dealloc: (a: number) => void;
    readonly __wbindgen_free: (a: number, b: number, c: number) => void;
    readonly __wbindgen_start: () => void;
}

export type SyncInitInput = BufferSource | WebAssembly.Module;

/**
 * Instantiates the given `module`, which can either be bytes or
 * a precompiled `WebAssembly.Module`.
 *
 * @param {{ module: SyncInitInput }} module - Passing `SyncInitInput` directly is deprecated.
 *
 * @returns {InitOutput}
 */
export function initSync(module: { module: SyncInitInput } | SyncInitInput): InitOutput;

/**
 * If `module_or_path` is {RequestInfo} or {URL}, makes a request and
 * for everything else, calls `WebAssembly.instantiate` directly.
 *
 * @param {{ module_or_path: InitInput | Promise<InitInput> }} module_or_path - Passing `InitInput` directly is deprecated.
 *
 * @returns {Promise<InitOutput>}
 */
export default function __wbg_init (module_or_path?: { module_or_path: InitInput | Promise<InitInput> } | InitInput | Promise<InitInput>): Promise<InitOutput>;
