purs_args := "--stash --censor-lib --censor-codes=ImplicitQualifiedImport"
cfg_test := "--config test.dhall"

build-strict:
    spago build --purs-args "--strict {{purs_args}}"

build:
    spago build --purs-args "{{purs_args}}"

test-strict:
    spago {{cfg_test}} test --purs-args "--strict {{purs_args}}"

test:
    spago {{cfg_test}} test --purs-args "{{purs_args}}"

clean:
    rm -rf .spago output .psa-stash

ide:
    spago {{cfg_test}} test --purs-args "{{purs_args}} --json-errors"

ci: clean build-strict test-strict


format:
    purs-tidy format-in-place "src/**/*.purs"
    purs-tidy format-in-place "test/**/*.purs"
    purs-tidy format-in-place "samples/**/*.purs"


check-format:
    purs-tidy check "src/**/*.purs"
    purs-tidy check "test/**/*.purs"
    purs-tidy check "samples/**/*.purs"

gen-ts:
    spago {{cfg_test}} run --purs-args "{{purs_args}}" --main TsBridge.Main
    yarn run prettier --write output/*/index.d.ts

check-ts:
    tsc