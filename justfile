
clean:
    rm -rf .spago output .psa-stash

ci: install check-format gen-ts test check-ts

test:
    spago test

format:
    purs-tidy format-in-place "src/**/*.purs"
    purs-tidy format-in-place "test/**/*.purs"

check-format:
    purs-tidy check "src/**/*.purs"
    purs-tidy check "test/**/*.purs"

gen-ts:
    spago run --main TsBridge.Main
    yarn run prettier --write output/*/index.d.ts

check-ts:
    tsc

install:
    yarn install