# Linux Provisioning

Simple installation of new machines.

## Running

The provisioning can be run by either pulling:

```bash
ansible-pull -KU https://github.com/joncol/linux-provisioning.git
```

Or by pushing:

```bash
just setup-box USERNAME [PORT]
just provision
```
