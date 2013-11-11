An aggregation of all online content related to Haskell.

## Build notes

You'll need a modified version of the `github` package. A recipe for building with `cabal sandbox`:

    git clone https://github.com/erantapaa/github.git
    (cd github && git checkout hn)
    cabal sandbox init
    cabal sandbox add-source github
    cabal install --only-dependencies
    cabal configure && cabal build

## Updating push events from Github:

    $ dist/build/haskellnews/haskellnews haskellnews.conf github

Also see the `GITHUB` section in the `haskellnews.conf.sample` file.

## Database setup

Create the PostgreSQL database:

    $ sudo su postgres --command 'createuser haskellnews -P'
    $ sudo su postgres --command 'createdb haskellnews -O haskellnews'

Update the database to the latest migration:

    $ dist/build/haskellnews/haskellnews haskellnews.conf --create-version

