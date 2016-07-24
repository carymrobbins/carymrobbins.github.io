---
title: Installing Guitar Pro on Linux
---

In my specific case, I used Ubuntu 14.04 and Guitar Pro 6.1.9 (r11686).

First, you'll need to download the Guitar Pro 6 deb file by logging
into the [Guitar Pro website](https://www.guitar-pro.com).

Next, drop into the terminal to extract the deb into a temp directory, `gp6-tmp` -

```text
% dpkg-deb -x ~/Downloads/gp6-full-linux-r11686.deb gp6-tmp
```

Now you'll need to extract the control information -

```text
% dpkg-deb --control ~/Downloads/gp6-full-linux-r11686.deb gp6-tmp/DEBIAN
```

For whatever reason, there are a couple of Mac `DS_Store` files
laying around in the archive, so let's remove those -

```text
% rm gp6-tmp/.DS_Store gp6-tmp/._.DS_Store
```

Now, open up the file `gp6-tmp/DEBIAN/control` in a text editor
and find the `Depends` line. According to
[this wiki post](https://doc.ubuntu-fr.org/guitarpro),
we need to remove `gksu` from the dependencies.

(Note: I'm not sure that you actually need to do this since we end
up needing to install `gksu` anyway to launch the updater.)

```text
% grep '^Depends' gp6-tmp/DEBIAN/control
Depends: gksu, libasound2, libc6 (>= 2.1.3), libglu1-mesa, libportaudio0, libportaudio2, libssl0.9.8, libstdc++6, libxml2, libxslt1.1, zlib1g

% vim gp6-tmp/DEBIAN/control # Make your edits to remove gksu

% grep '^Depends' gp6-tmp/DEBIAN/control
Depends: libasound2, libc6 (>= 2.1.3), libglu1-mesa, libportaudio0, libportaudio2, libssl0.9.8, libstdc++6, libxml2, libxslt1.1, zlib1g
```

Now we can build our modified deb and install it -

```text
% dpkg -b gp6-tmp gp6-modified.deb
dpkg-deb: building package `guitarpro6' in `gp6-modified.deb'.

% sudo dpkg -i --force-architecture gp6-modified.deb
```

Note that you will probably receive the following errors -
```text
dpkg: dependency problems prevent configuration of guitarpro6:
 guitarpro6 depends on libglu1-mesa.
 guitarpro6 depends on libportaudio0.
 guitarpro6 depends on libportaudio2; however:
  Package libportaudio2:i386 is not installed.
 guitarpro6 depends on libssl0.9.8; however:

dpkg: error processing package guitarpro6 (--install):
 dependency problems - leaving unconfigured
Processing triggers for gnome-menus (3.10.1-0ubuntu2) ...
Processing triggers for desktop-file-utils (0.22-1ubuntu1) ...
Processing triggers for bamfdaemon (0.5.1+14.04.20140409-0ubuntu1) ...
Rebuilding /usr/share/applications/bamf-2.index...
Processing triggers for mime-support (3.54ubuntu1.1) ...
Errors were encountered while processing:
 guitarpro6
```

The following will have `apt-get` fix the broken package -

```text
% sudo apt-get install -f
```

From here I was able to launch Guitar Pro just fine. If you have problems launching,
you can find the launcher script in `/opt/GuitarPro6/launcher.sh`

But there's still one more problem - sound doesn't seem to work!
I'm not sure how to get MIDI output configured,
but using the RSE works just fine, you just need to update Guitar Pro.
In order to do that, we need to install `gksu` (which is why I question
whether or not we actually needed to remove it from the deps in the first place) -

```text
% sudo apt-get install gksu
```

If we don't, we'll get the following error when attempting to update -

```text
Failed to start Guitar Pro Updater
```

Once you have `gksu` installed, open Guitar Pro and choose **Help > Launch Updater** to update
and install the RSE sound banks. Once that completes, restart Guitar Pro
and audio playback should work.
