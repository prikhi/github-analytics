# Github Analtics

I wanted an application that would pull all the traffic data from my github
repositories & show me a graph & table of total pageviews & clones, along with
a breakdown per repository.

![A screenshot of the Github Analytics application, showing a table of repositories and their view counts from the last 14 days.](/screenshot.png?raw=true "Github Analytics")

This is project is built with Haskell's
[reflex-platform](https://github.com/reflex-frp/reflex-platform), mostly
because I wanted to try it out.

## Run

First install nix, it's easiest to do via the `reflex-platform`:

    $ git submodule update --recursive --init
    $ reflex-platform/try-reflex
    [nix-shell] $ exit

You might need to add something like `source
~/.nix-profile/etc/profile.d/nix.sh` to your `.bashrc`.

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

Changes to `common/` will require restarting `ghci`. The client repl also has a
`mainServer` function that will allow you build with GHC & connect with your
browser to `http://localhost:3911`.


`./manage.hs nix-build` will make production builds of the server and the web,
gtk, & mobile clients. This will take a while.

You can get faster, incremental builds using `./manage.hs ghc-build` &
`./manage.hs ghcjs-build`.

## TODO

* Add graphs (probably using `diagrams-reflex`?), requires returning views per day
* More data: referrers, pages, clones, unique views
* Add organizations - with checkboxes to toggle on & off
* Add filters - affiliation, type(fork/source), language, etc
* Add table sorting
* Figure out proper XHR error handling(use `WithError` function)
* Style it
* Add needed Github data types & endpoints to `phadej/github` package instead
  of writing it ourselves.
* Make it permanent!

    * After receiving user/pass, make & store github API token & use that instead.
      Would be really cool to give users option to encrypt token w/ password
      and have them submit password when they want to update. Wouldn't let us
      automatically pull traffic data, but much more secure.
    * Save user's historical per-day data(maybe monthly data for people who aren't me)
      - github only gives us the last 14 days.
    * Cron job to update data every day? Would need to store api tokens unencrypted.


## License

GPL-3.0
