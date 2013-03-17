An aggregation of all online content related to Haskell.

## Database setup

Create the PostgreSQL database:

    $ sudo su postgres --command 'createuser haskellnews -P'
    $ sudo su postgres --command 'createdb haskellnews -O haskellnews'

Update the database to the latest migration:

    $ dist/build/haskellnews/haskellnews haskellnews.conf --create-version
