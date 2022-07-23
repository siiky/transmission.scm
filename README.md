# transmission.scm

[Transmission][transmission] (v3.01) RPC (v17, [rpc-spec.txt][rpc-spec]) in
Scheme (CHICKEN 5). For the docs see the [CHICKEN wiki].

The versions above are the versions that are (supposedly) guaranteed to work.
They don't mean a later RPC Spec isn't supported. Some changes made to the spec
require no change in the API, because they're usually on the server (daemon)
side. An example of this is:

```
17    | 3.01    | yes       | torrent-get          | new arg "file-count"
17    | 3.01    | yes       | torrent-get          | new arg "primary-mime-type"
```

Neither requires an API change because the library doesn't check if the fields
are valid (this is intentional).

Changes like these, however:

```
16    | 3.00    | yes       | session-get          | new request arg "fields"
16    | 3.00    | yes       | torrent-get          | new request arg "format"
```

Require API changes.

## Trying it out!

Install dependencies and compile:

```sh
make
# Start a transmission-daemon for playing with.
# See the Makefile for more details.
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
`kebab-case`. The exceptions are a few arguments in `camelCase`; these are
converted to `kebab-case` in this egg: e.g., the key argument for
`queuePosition` is called `queue-position`. Note, however, that the messages
are left untouched: a message to/from the server will still use `queuePosition`
as the key, **NOT** `queue-position`.

### Core Procedures

#### `update-request-session-id`

Updates a `request` object's `X-Transmission-Session-Id` header. See section
**2.3.1** of the [spec][rpc-spec].

#### `handle-409`

Handles an exception caused by a `409` error. See section **2.3.1** of the
[spec][rpc-spec].

#### `make-message`

Create a Scheme representation of an RPC message.

#### `serialize-message`

Serialize an RPC message to JSON.

#### `make-serialized-message`

Create and serialize an RPC message.

#### `make-rpc-request`

Convenience wrapper for `intarweb`'s `make-request`.

#### `rpc-call`

The core procedure to make RPC calls. See the source for details.

## Contributing

This egg is not very well tested (yet). If you have found a bug, a typo, or
have any suggestions, open an issue or make a pull request.

## Resources

 * [RPC Specification][rpc-spec]
 * [RPC Related Settings][rpc-config]

[CHICKEN wiki]: https://wiki.call-cc.org/eggref/5/transmission
[rpc-config]: https://github.com/transmission/transmission/blob/6e1b89d9a7bc2e1cf40884d67fbcef3968ed2ff0/docs/Editing-Configuration-Files.md#rpc
[rpc-spec]: https://github.com/transmission/transmission/blob/6e1b89d9a7bc2e1cf40884d67fbcef3968ed2ff0/docs/rpc-spec.md#231-csrf-protection
[transmission]: https://github.com/transmission/transmission
