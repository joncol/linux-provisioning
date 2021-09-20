# Linux Provisioning

Simple installation of new machines.

## Prerequisites

To decrypt the ansible vault, you need the `lastpass-cli`
package. To use the `justfile`, the package `just` is needed.

You also need to have `ansible` installed.

Also install the `ansible-galaxy` requirements via:

```bash
ansible-galaxy install -r requirements.yml
```

## Running

The provisioning can be run for the local machine by:

```bash
just setup-local
just provision
```
