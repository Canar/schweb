# schweb

*by Benjamin Cook*\
<http://baryon.it>

## Abstract

An exploration in static web generation using Scheme.

## Platforms 

- Chicken Scheme
- Chibi Scheme
- Guile Scheme

### Anti-Platforms

- MIT/GNU Scheme: can't get a working cond-expand? The code that works for Chicken, Chibi, and Guile breaks for some reason in MIT/GNU.

## Usage

Doesn't even do anything yet except run some tests.

`./scheme [platform] [code.ss] [args]` is a front-end to invoke supported platforms.

- `chicken`, `n`: Chicken
- `chibi`, `b`: Chibi
- `guile`, `g`: Guile
- `all`, `a`: All platforms sequentially

The only argument right now is `--test` which runs tests.

`./scheme a union.ss --test` will run all tests.

The `./scheme` script is coded to point at Gentoo's executable names, may vary.

## Release Log

2025-09-11 - Uploaded to Github for synchronization reasons
2025-09-12 - Learning to make multi-plat Scheme work, `./scheme` bash launcher
