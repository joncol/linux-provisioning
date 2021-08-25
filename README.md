# Linux Provisioning

Simple installation of new machines.

## Prerequisites

To decrypt the ansible vault, `direnv` is needed. Also, you need the `lpass-cli`
package.

## Running

The provisioning can be run by either pulling:

```bash
ansible-pull -KU https://github.com/joncol/linux-provisioning.git
```

Or by pushing:

```bash
just setup-box USERNAME IP [PORT]
just provision
```
