# share-fs

A share file system base on http and fuse

# Build

Recommand build `sharefs` with [`stack`](https://docs.haskellstack.org/en/stable/README/)

    git clone https://github.com/Lupino/sharefs.git
    stack build
    stack install

# Quick start

    # start server
    share-fs-server -H 127.0.0.1 -p 8080 --path share-fs

    # start fuse client
    mkdir -p mount
    share-fs-fuse -c config.yml

# API

See [docs](https://lupino.github.io/sharefs/)
