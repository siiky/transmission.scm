# Build Variables

EGG_SRC := transmission.scm
EXAMPLE_SRC := example.scm
SRC := $(EGG_SRC) $(EXAMPLE_SRC)

default: $(EGG_SRC)
	chicken-install -n

install: $(EGG_SRC)
	chicken-install

clean:
	chicken-clean

lint: $(SRC)
	chicken-lint $(SRC)

# Transmission Daemon Settings

AUTH_NO_AUTH := --auth # --no-auth
CONFIG_DIR := transmission_config
DHT_NO_DHT := --no-dht # --dht
DOWNLOAD_DIR := transmission_download
ENCRYPTION := --encryption-required
LOGGING := --log-debug
LPD_NO_LPD := --no-lpd # --lpd
PASSWORD := password
PEER_PORT := 50000
PORTMAP_NO_PORTMAP := --no-portmap # --portmap
RPC_PORT := 4242
USERNAME := username

LOG_FILE := transmission.log

TRANSMISSION_ARGS := \
    $(AUTH_NO_AUTH) \
    $(ENCRYPTION) \
    $(LOGGING) \
    $(PORTMAP_NO_PORTMAP) \
    --config-dir $(CONFIG_DIR) \
    --download-dir $(DOWNLOAD_DIR) \
    --no-incomplete-dir \
    --no-watch-dir \
    --password $(PASSWORD) \
    --peerport $(PEER_PORT) \
    --port $(RPC_PORT) \
    --username $(USERNAME) \
    -f \

CSI_ARGS := \
    --username $(USERNAME) \
    --password $(PASSWORD) \
    --port $(RPC_PORT) \

$(DOWNLOAD_DIR):
	mkdir $(DOWNLOAD_DIR)

daemon_running:
	[ -f start-transmission ] # transmission is not running; run `make start-transmission`

csi: $(EXAMPLE_SRC) daemon_running
	csi -q -s example.scm --repl $(CSI_ARGS)

example: $(EXAMPLE_SRC) daemon_running
	csi -q -s example.scm $(CSI_ARGS)

start-transmission: $(DOWNLOAD_DIR)
	touch start-transmission
	transmission-daemon $(TRANSMISSION_ARGS) 2>&1 | tee $(LOG_FILE)
	rm start-transmission

.PHONY: clean csi daemon_running default example lint
