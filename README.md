# transmission.scm

[Transmission][transmission] (v3) [RPC][rpc-wiki] (v16) in Scheme (CHICKEN 5).

## Trying it out!

Install dependencies and compile:

```sh
make
# start a transmission-daemon for playing with
# see the Makefile for more details
# ^C or `(session-close)` in csi to stop
make start-transmission
```

In another terminal, run the example or `csi`:

```sh
chicken-install optimism

make example # run the example script
# or
make csi # start csi ready to play with the daemon started above
```

See `transmission.log` for `transmission-daemon`'s logs (and `LOG_FILE` in the
`Makefile`).

## API

### RPC Settings

| transmission.scm | RPC Settings   | Default                     |
| :--------------: | :------------: | :-------------------------: |
| `*url*`          | `rpc-url`      | `'(/ "transmission" "rpc")` |
| `*port*`         | `rpc-port`     | `9091`                      |
| `*username*`     | `rpc-username` | `#f`                        |
| `*password*`     | `rpc-password` | `#f`                        |

### `*url*`

`*url*` is not `rpc-url` directly, but is derived from it. E.g., if
`rpc-url="/some/path/"`, then `*url*` should be `'(/ "some" "path" "rpc")`.

### `*host*`

The hostname of the Transmission RPC server. Defaults to `localhost`.

### `*session-id*`

The `X-Transmission-Session-Id` header. See section **2.3.1** of the [spec][rpc-spec].

This parameter is optional. If not set, Transmission will reply with a `409`,
and the correct `X-Transmission-Session-Id` will be set.

### RPC Methods

Every method of the [spec][rpc-spec] is defined, and naming is followed almost
directly. In the [spec][rpc-spec], all methods and most arguments follow
`kebab-case`. The exceptions are a few arguments in `camelCase`. These are
converted to `kebab-case` in this egg. E.g. `queuePosition` is converted to
`queue-position`.

### Core Procedures

#### `update-request-session-id`

Updates a `request` object's `X-Transmission-Session-Id` header. See section
**2.3.1** of the [spec][rpc-spec].

#### `handle-409`

Handles an exception caused by a `409` error. See section **2.3.1** of the
[spec][rpc-spec].

#### `serialize-message`

Serialize an RPC message to JSON.

#### `make-rpc-request`

Convenience wrapper for `intarweb`'s `make-request`.

#### `rpc-call`

The core procedure to make RPC calls. See the source for details.

## Contributing

This egg is not very well tested (yet). If you have found a bug, a typo, or
have any suggestions, open an issue or make a pull request.

## Resources

 * [RPC Wiki Page][rpc-wiki]
 * [RPC Specification][rpc-spec]
 * [RPC Related Settings][rpc-config]

[rpc-config]: https://github.com/transmission/transmission/wiki/Editing-Configuration-Files#rpc
[rpc-spec]: https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
[rpc-wiki]: https://github.com/transmission/transmission/wiki/RPC-Protocol-Specification
[transmission]: https://github.com/transmission/transmission
