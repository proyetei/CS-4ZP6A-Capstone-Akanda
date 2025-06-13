# Running the test suite

You can run the test suite with

```sh
cabal test translators
```

This will output files to `translator/test/staging/`, which are then
diffed against the corresponding file in `translator/test/snapshot`.

To promote output file in the staging area to snapshots, run

```sh
cabal test translator --test-options="--accept"
```

If you add a new golden test, the first run of the test suite
will create a new snapshot file. Make sure to commit this 
file as part of your PR!
