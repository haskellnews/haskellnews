[![Build Status](https://travis-ci.org/haskellnews/haskellnews.svg?branch=master)](https://travis-ci.org/haskellnews/haskellnews)

## Overview

[haskellnews.org] is an aggregation of all online content related to Haskell.

> I think this paints a fairly comprehensive picture of the Haskell community’s
> public activities.
>
> Certainly, if you want Haskell news, here is the best place online to go.
>
> [Haskell News] - Chris Done

## Features

  - [Grouped], or [mixed] content viewing.
  - [Mixed] RSS feed.

All content is updated every 10 minutes.

Want more features or sources integrated? [Open an issue!]

## Contributing

[Fork the repository] and use [Git] to clone your copy:

    $ git clone https://github.com/<username>/haskellnews/

Using [Stack], build the project:

    $ stack build

Note. If you are a [Cabal] user, please help out and submit a PR with the
relevant Cabal commands to get started. Much appreciated!

Meanwhile, you can create a local copy of the necessary configuration:

    $ cp haskellnews.conf.sample haskellnews.conf

Create a development PostgreSQL database:

    $ sudo su postgres --command 'createuser haskellnews -P'
    $ sudo su postgres --command 'createdb haskellnews -O haskellnews'

Then, create the initial version:

    $ stack exec haskellnews haskellnews.conf create-version

Import Haskell content with:

    $ stack exec haskellnews haskellnews.conf github
    $ stack exec haskellnews haskellnews.conf import

Note. Your `haskellnews` database now has a `public` table full of `item`s.

And finally, run the server:

    $ stack exec haskellnews haskellnews.conf

[Fork the repository]: https://github.com/chrisdone/haskellnews/issues#fork-destination-box
[Git]: https://git-scm.com/
[Stack]: https://docs.haskellstack.org/en/stable/README/
[Cabal]: https://www.haskell.org/cabal/
[Haskell News]: http://chrisdone.com/posts/haskell-news
[haskellnews.org]: http://haskellnews.org/
[Open an issue!]: https://github.com/chrisdone/haskellnews/issues
[Grouped]: http://haskellnews.org/grouped
[mixed]: http://haskellnews.org/mixed
[Mixed]: http://haskellnews.org/feed
