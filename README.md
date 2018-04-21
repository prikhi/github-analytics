# Github Analtics

I wanted an application that would pull all the traffic data from my github
repositories & show me a graph & table of total pageviews & clones, along with
a breakdown per repository.

This is project is built with Haskell's
[reflex-platform](https://github.com/reflex-frp/reflex-platform), mostly
because I wanted to try it out.

## Run

First install nix, it's easiest to do via the `reflex-platform`:

    $ git submodule update --recursive --init
    $ reflex-platform/try-reflex
    [nix-shell] $ exit

The easiest way to start developing is launching GHCi to run the server & GTK
client:

    # Start the server:
    $ ./manage.hs server-repl
    > main
    # In another terminal:
    $ ./manage.hs client-repl
    > main
    # After making some changes:
    > :r
    > main

Changes to `common/` require restarting `ghci`. The client repl also has a
`mainServer` function that will allow you build with GHC & connect with your
browser to `http://localhost:3911`.


`./manage.hs nix-build` will make production builds of the server and the web,
gtk, & mobile clients. This will take a while.

You can get faster, incremental builds using `./manage.hs ghc-build` &
`./manage.hs ghcjs-build`.

## TODO

* Add graphs (probably using `diagrams-reflex`?), requires returning views per day
* More data: referrers, pages, clones, unique views
* Style it
* Make it permanent!

    * After receiving user/pass, make & store github API token & use that instead
    * Save user's historical per-day data(maybe monthly data for people who aren't me)
      - github only gives us the last 14 days.
    * Cron job to update data every day? Would need to store api tokens unencrypted.


## License

GPL-3.0
