## The OSDev Wiki

The goal of this wiki is to modernize the information found in the long-standing OSDev wiki found at [wiki.osdev.org](https://wiki.osdev.org) and to decentralize it. It is currently **WIP** and **not ready** for use.

This wiki is not endorsed by, does not have any connection to, and is not related in any way with another wiki found at [wiki.osdev.org](https://wiki.osdev.org).

# Contributing

## Installing dependencies

This wiki is written in Haskell.

The project has some non-Haskell dependencies:

- [Pygments](https://github.com/pygments/pygments) - `pip install Pygments`
- [AsciiDoctor](https://asciidoctor.org/) - `gem install asciidoctor`
- [asciidoctor-bibtex](https://github.com/asciidoctor/asciidoctor-bibtex) - `gem install asciidoctor-bibtex`
- [stork](https://stork-search.net/docs/install) - `cargo install stork-search --locked`

## Building and running

Because building the Haskell dependecies takes a long time, we recommend using our Nix binary cache to get all the dependecies without building. (~10 min setup time)

### With Nix

[Install nix](https://nixos.org/download.html#download-nix).

Edit the configuration file added by Nix at `/etc/nix/nix.conf` and add these two lines to setup the binary cache:

```nix
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= osdev-wiki-cache-2:xnfH8Tkm0Sp5c8dDxpuFE0/w1PB6E4NUxjcnShNdkZ0=
substituters = https://cache.nixos.org/ https://hydra.iohk.io s3://osdev-wiki-cache?scheme=https&endpoint=s3.us-west-000.backblazeb2.com
```

From the root of the project, build:

```shell
cd generator
nix-build
```

Run the generator:

```shell
cd ..
./generator/result/bin/generator watch
```

### Building locally

**Alternatively**, you can build locally (~1 hour build time). You do _not_ need to do this if you have done the Nix setup above.

You will need to have Stack installed to compile and run the main binary.

From the root of the project, run:

```shell
cd generator
stack build
```

Run the generator:

```shell
stack run --cwd .. watch
```

## Writing content

All content can be found in `pages/`. You can find our [quick guide here](https://osdev.wiki/pages/writer_tutorial.html) to learn the basics (~2 min reading).
