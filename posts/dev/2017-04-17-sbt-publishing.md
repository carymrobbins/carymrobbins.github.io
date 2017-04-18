# Publishing with SBT

```
> pgp-cmd gen-key
java.lang.RuntimeException: Cannot modify keyrings when in read-only mode.  Run `set pgpReadOnly := false` before running this command.
```

Oh, better do what the man says.

```
> set pgpReadOnly := false
...

> pgp-cmd gen-key
Please enter the name associated with the key: Cary Robbins
Please enter the email associated with the key: cary@example.com
Please enter the passphrase for the key: ********************************
Please re-enter the passphrase for the key: ********************************
[info] Creating a new PGP key, this could take a long time.
[info] Public key := /home/user/.sbt/gpg/pubring.asc
[info] Secret key := /home/user/.sbt/gpg/secring.asc
[info] Please do not share your secret key.   Your public key is free to share.

> set pgpReadOnly := true
...
```

Seems I can't publish without credentials, so I'll need a Sonatype account.

Wait...in order to do that I need to submit a JIRA issue; however, I don't have an
account for Sonatype JIRA, so I'll need to get one of those.

Ok, got it, now open an issue on JIRA to get a new account (and possibly create the
new project).

https://issues.sonatype.org/browse/OSSRH-30654
