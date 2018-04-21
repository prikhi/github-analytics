# Github Analtics

I wanted an application that would pull all the traffic data from my github
repositories & show me a graph & table of total pageviews & clones, along with
a breakdown per repository.

This is project is built with Haskell's
[reflex-platform](https://github.com/reflex-frp/reflex-platform), mostly
because I wanted to try it out. 

* request user/pass from user(if made into multi-user app - make auth token)
* pull all repositories, filter out forks(see repositories api docs)
* fetch every type of traffic info for each repository(see repositories/traffic api docs).
* save per-day data for previous dates to database(so eventually will have more
  than the 14 day limit github gives us).
* send to frontend & graph(checkout `diagrams-reflex`)
* cron job to update daily

## Run

First install nix, it's easiest to do via the `reflex-platform`:

    $ git submodule update --recursive --init
    $ reflex-platform/try-reflex
    [nix-shell] $ exit

The easiest way to start developing is launching GHCi & running the GTK client:

    $ ./manage.hs client-repl
    > main
    # After making some changes: 
    > :r
    > main

`./manage.hs nix-build` will build the server and the web, gtk, & mobile clients.
This will take a while.

You can get incremental builds using `./manage.hs ghc-build` & `./manage.hs
ghcjs-build`.

## License

GPL-3.0
