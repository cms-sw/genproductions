# README

## Get VM

* Get VM at https://openstack.cern.ch
* Use CS8 image

## Basic setup

```
dnf install dnf-autoupdate bash-completion nano
nano /etc/dnf/automatic.conf -> apply_updates = yes
systemctl enable --now dnf-automatic.time

adduser runner

dnf install locmap-release
dnf install locmap
locmap --enable cvmfs
locmap --configure all
```

## Docker

```
yum-config-manager --add-repo https://download.docker.com/linux/centos/docker-ce.repo
dnf install docker-ce docker-ce-cli containerd.io docker-compose-plugin

usermod -aG docker runner

systemctl enable --now docker.service
```

## Gitlab runner

Can be done multiple times in different directories for registering multiple runners on the same VM.

### Install+register as `runner`
* Follow instructions from https://github.com/cms-sw/genproductions/settings/actions/runners
* To cope with timeouts in registering the runner, use `--unattended`:
```
./config.sh --url https://github.com/cms-sw/genproductions --token <token> --unattended --replace --name <name>
```

### Enable as a service as `root`:
```
./svc.sh install runner
chcon system_u:object_r:usr_t:s0 runsvc.sh
./svc.sh start
```

After a few minutes, new runner should show up as up and "Idle".
