# transmission.scm

[Transmission][transmission] (v3) [RPC][rpc-wiki] (v16) in Scheme

## Example

```sh
chicken-install -n
csi -s example.scm your-rpc-username your-rpc-password
```

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

This parameter is optional. If not set, Transmission will reply with a `409`, and the correct
`X-Transmission-Session-Id` will be set.

### `rpc-call`

The core procedure to make RPC calls. See the source for details.

### 3.1 Torrent Action

 * `(torrent-reannounce #!key tag ids)`
 * `(torrent-start      #!key tag ids)`
 * `(torrent-start-now  #!key tag ids)`
 * `(torrent-stop       #!key tag ids)`
 * `(torrent-verify     #!key tag ids)`

## Resources

 * [RPC Wiki Page][rpc-wiki]
 * [RPC Specification][rpc-spec]
 * [RPC Related Settings][rpc-config]

[rpc-config]: https://github.com/transmission/transmission/wiki/Editing-Configuration-Files#rpc
[rpc-spec]: https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt
[rpc-wiki]: https://github.com/transmission/transmission/wiki/RPC-Protocol-Specification
[transmission]: https://github.com/transmission/transmission
