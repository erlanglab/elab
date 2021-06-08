elab
=====

A collection of libraries and notebooks useful for troubleshooting and profiling of BEAM-based systems.

Install prerequisites
=====

Install asdf
-----

    $ git clone https://github.com/asdf-vm/asdf.git ~/.asdf
    $ # add it to your shell resource file, e.g. ~/.zshrc
    $ . $HOME/.asdf/asdf.sh

Install Erlang
-----

    $ export KERL_CONFIGURE_OPTIONS="--without-javac --enable-lock-counter --with-microstate-accounting=extra"
    $ asdf install erlang 24.0.2
    $ asdf global erlang 24.0.2

Install Elixir and Livebook (optional)
-----

    $ asdf install elixir 1.12.1-otp-24
    $ asdf global elixir 1.12.1-otp-24
    $ git clone https://github.com/elixir-nx/livebook.git
    $ cd livebook
    $ mix deps.get --only prod
    $ mix escript.install hex livebook
    $ # add livebook to your $PATH
    $ export PATH="$HOME/.asdf/installs/elixir/1.12.1-otp-24/.mix/escripts/:$PATH" 

Build
-----

    $ rebar3 compile
