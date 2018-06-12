OCaml and opam are community-orientated open source projects, and
the build infrastructure around it is designed to be openly accessible,
within the limits of security required by our infrastruture.
Accordingly, this repository publishes the code that powers the
bulk build instructure, and the results of the runs are available
in the <obi-logs> repository under a liberal CC0 license. 

It was originally developed at OCaml Labs and is now maintained by
OCaml Labs, Tarides as well as developers from the OCaml and opam
communities.

Contributions to Obi are welcome and should be submitted via GitHub
pull requests. Obi code is distributed under the ISC license and
contributors are required to sign their work in order to certify that
they have the right to submit it under this license. See the following
section for more details.

Signing contributions
---------------------

We require that you sign your contributions. Your signature certifies
that you wrote the patch or otherwise have the right to pass it on as
an open-source patch. The rules are pretty simple: if you can certify
the below (from [developercertificate.org][dco]):

```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.


Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

Then you just add a line to every git commit message:

```
Signed-off-by: Joe Smith <joe.smith@email.com>
```

Use your real name (sorry, no pseudonyms or anonymous contributions.)

If you set your `user.name` and `user.email` git configs, you can sign
your commit automatically with `git commit -s`.

