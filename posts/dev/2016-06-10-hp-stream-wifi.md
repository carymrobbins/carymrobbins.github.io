---
title: Fixing Linux Wifi on an HP Stream laptop
---

**TL;DR** - The following gets wifi working well on the HP Stream laptop running
Ubuntu 14.04.4 LTS -

```
$ sudo add-apt-repository ppa:hanipouspilot/rtlwifi
$ sudo apt-get update
$ sudo apt-get install rtlwifi-new-dkms linux-firmware
$ sudo modprobe -r rtl8723be
$ sudo modprobe -v rtl8723be isp=0 ant_sel=2
$ echo 'options rtl8723be isp=0 ant_sel=2' | sudo tee /etc/modprobe.d/rtl8723be.conf
```

---------------------

I purchased an [HP Stream laptop](http://store.hp.com/us/en/mdp/Laptops/hp-stream-notebook#!&TabName=vao&jumpid=ps_gymaw6xezr&aoid=51319&002=2190637&004=20637427508&005=104331204308&006=123108661148&007=Search&008=&025=c&026=)
for my daughter this past Christmas and she has loved it.  However, I noticed that it seemed to
run awfully slow for her most of the time.  She had gotten relatively accustomed to it and,
naturally, this was a nuisance to me.  I convinced her that we should switch to Linux and see
how it does.  We installed Ubuntu 14.04.4 LTS and performance was great!  She instantly
noticed the difference and became very excited.  However, as always,
there was some work to be done.  Wifi was very inconsistent and the signal was extremely low.

I discovered that there was an [rtlwifi](https://github.com/lwfinger/rtlwifi_new) driver available
that we could build from source; however, this didn't seem to improve anything.  Later, I
realized that this driver was also managed in an apt repo, so I quickly switched to use the
PPA -

```
$ sudo add-apt-repository ppa:hanipouspilot/rtlwifi
$ sudo apt-get update
$ sudo apt-get install rtlwifi-new-dkms linux-firmware
```

Of course, this didn't really help things - signal was still very low and the connection
would regularly disconnect.  Upon perusing various forums, I found that someone mentioned
[selecting Antenna 2](http://h30434.www3.hp.com/t5/Notebook-Wireless-and-Networking/Issues-with-HP-Stream-11/td-p/5461096).
It took me a while (in spite of the github issue link) to realize how to do this.  The
key is in setting `ant_sel=2` in the driver configuration.  We can test it immediately
without having to reboot -

```
$ sudo modprobe -r rtl8723be
$ sudo modprobe -v rtl8723be isp=0 ant_sel=2
```

Like magic, once loaded, the wifi signal was very strong and worked as expected.
To make the configuration permanent upon reboot, we just need to add our settings
to `/etc/modprobe.d/rtl8723be.conf` -

```
$ echo 'options rtl8723be isp=0 ant_sel=2' | sudo tee /etc/modprobe.d/rtl8723be.conf
```
