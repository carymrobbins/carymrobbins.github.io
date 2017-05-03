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

---------------------------

Once all that was done, and I got build.sbt configured correctly, running `release` from
sbt didn't quite work just yet.

> No tracking branch is set up. Either configure a remote tracking branch, or remove the pushChanges release part.

This [issue](https://github.com/sbt/sbt-release/issues/100) seems to suggest that
we can configure this with git directly. First, I checked the output of each command without
the last argument to see its value. The `remote.origin.fetch` seemed to already be set
properly, so I just set the last two. However, here are all three for reference -

```
git config remote.origin.fetch +refs/heads/*:refs/remotes/origin/*
git config branch.master.remote origin
git config branch.master.merge refs/heads/master
```

Let's try running `release` again...

```
> release
[info] Starting release process off commit: be71b18e16fb0bd8738eb1d9aba26665be17c256
[info] Checking remote [origin] ...
Release version [0.0.1] :
Next version [0.0.2-SNAPSHOT] :
```

Hey! Look at that, it worked! And to boot, it also inferred the appropriate version bumps.

It then proceeded to run my tests, publish to the release repository, then prompt
me to push the changes -

```
Push changes to the remote repository (y/n)? [y]
[info] To github.com:estatico/confide.git
[info]    be71b18..c352b18  master -> master
[info] To github.com:estatico/confide.git
[info]  * [new tag]         v0.0.1 -> v0.0.1
```

Sweet!

----------------------------------------

So I needed to then go to oss.sonatype.org and in the UI _close_ and _release_ my
staging repository.

So close, but seems I dind't upload my public key to one of the servers! Let me
try that...

```
> pgp-cmd send-key 0dd68733 hkp://pool.sks-keyservers.net
```

I could just try to re-close and release it directly on Sonatype, but let me see
if I can automate the process from SBT.

Ok now I need to log into Sonatype and drop my staging repository and re-publish it.

```
> +publishSigned
```

Ok, now let's add the sonatype plugin to our `project/plugins.sbt` file.

```
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "1.1")
```

Credentials **have** to go in ~/.sbt/0.13/sonatype.sbt, don't put it in plugins/, won't work!

Then all is well -

```
> sonatypeReleaseAll io.estatico
[info] Nexus repository URL: https://oss.sonatype.org/service/local
[info] sonatypeProfileName = io.estatico
[info] Reading staging repository profiles...
[info] Reading staging profiles...
[info] Closing staging repository [ioestatico-1001] status:open, profile:io.estatico(5cb4e883b06886) description: Implicitly created (auto staging).
[info] Activity open started:2017-04-21T23:24:07.497Z, stopped:2017-04-21T23:24:11.712Z
[info] repositoryCreated: id:ioestatico-1001, user:cary, ip:70.158.101.154
[info] Activity close started:2017-04-24T02:32:59.546Z, stopped:
[info]   Evaluate: id:5e9e8e6f8d20a3, rule:sources-staging
[info]   Evaluate: javadoc-staging
[info]     Passed: javadoc-staging
[info]   Evaluate: pom-staging
[info]     Passed: pom-staging
[info]   Evaluate: signature-staging
[info]     Passed: signature-staging
[info]   Evaluate: sources-staging
[info]     Passed: sources-staging
[info]   Evaluate: checksum-staging
[info]     Passed: checksum-staging
[info]     Passed: id:5e9e8e6f8d20a3
[info]      email: to:cary@example.com
[info] repositoryClosed: id:ioestatico-1001
[info] Closed successfully
[info] Promoting staging repository [ioestatico-1001] status:closed, profile:io.estatico(5cb4e883b06886) description: Implicitly created (auto staging).
[info] Activity release started:2017-04-24T02:33:21.175Z, stopped:
[info]   Evaluate: id:5e9e8e6f8d20a3, rule:sources-staging
[info]   Evaluate: javadoc-staging
[info]     Passed: javadoc-staging
[info]   Evaluate: pom-staging
[info]     Passed: pom-staging
[info]   Evaluate: signature-staging
[info]     Passed: signature-staging
[info]   Evaluate: sources-staging
[info]     Passed: sources-staging
[info]   Evaluate: checksum-staging
[info]     Passed: checksum-staging
[info]     Passed: id:5e9e8e6f8d20a3
[info]   Evaluate: id:nx-internal-ruleset, rule:RepositoryWritePolicy
[info]   Evaluate: RepositoryWritePolicy
[info]     Passed: RepositoryWritePolicy
[info]     Passed: id:nx-internal-ruleset
[info]  copyItems: source:ioestatico-1001, target:releases
[info]      email: to:cary@example.com
[info] repositoryReleased: id:ioestatico-1001, target:releases
[info] Promoted successfully
[info] Dropping staging repository [ioestatico-1001] status:released, profile:io.estatico(5cb4e883b06886) description: Implicitly created (auto staging).
[info] Dropped successfully: ioestatico-1001
```

Ok, better yet, we can try to update the `releaseProcess` so that the SBT `release` command
will just do it all -

```sbt
import ReleaseTransformations._

// ...

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts,
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _)),
  pushChanges
)
```
