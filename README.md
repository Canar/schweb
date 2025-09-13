# schweb

*by Benjamin Cook*\
<http://baryon.it>

## Abstract

An exploration in static web generation using Scheme.

## Usage

Doesn't even do anything yet except run some tests.

`./platform [platform] schweb.ss [arguments]` is a front-end to invoke supported platforms.

### Platforms

- `chicken`, `n`: Chicken
- `chibi`, `b`: Chibi
- `guile`, `g`: Guile
- `mit`, `m`: MIT/GNU Scheme
- `tiny`, `t`: Tinyscheme
- `all`, `a`: All platforms sequentially

### Arguments

The only argument right now is `--test` which runs tests.

`./ a union.ss --test` will run all tests.

The `./scheme` script is coded to point at Gentoo's executable names, may vary.

## Release Log

2025-09-11 - Uploaded to Github for synchronization reasons
2025-09-12 - Learning to make multi-plat Scheme work, `./scheme` bash launcher
