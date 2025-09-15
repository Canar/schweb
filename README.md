# schweb

*by Benjamin Cook*\
<http://baryon.it>

## Abstract

An exploration in static web generation using Scheme.

## Usage

Doesn't even do anything yet except run some tests.

`./platform [platform] schweb.ss [arguments]` is a front-end to invoke supported platforms.

### Platforms

- `chicken`, `n`: Chicken (interpreter)
- `chibi`, `b`: Chibi
- `guile`, `g`: Guile
- `mit`, `m`: MIT/GNU Scheme
- `sig`, `s`: SigScheme
- `tiny`, `t`: Tinyscheme
- `all`, `a`: All platforms, shuffled

### Arguments

The only argument right now is `--test` which runs tests.

`./platform a schweb.ss --test` will run all tests.

The `./platform` script is coded to point at Gentoo's executable names, may vary.

### Antiplatforms

- Sig's concern about special character symbols might be a concern, we'll see
- Chez won't ignore procedures that don't concern it, can't support without refactor

## Release Log

2025-09-11 - Uploaded to Github for synchronization reasons
2025-09-12 - Learning to make multi-plat Scheme work, `./scheme` bash launcher
2025-09-14 - SigScheme support added, renamed some files.
