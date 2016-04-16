Store EIEIO objects using EmacSQL
=================================

Store uniform [EIEIO] objects in an [EmacSQL] database.  [SQLite] is
used as backend.  This library imposes some restrictions on what kind
of objects can be stored; it isn't intended to store arbitrary
objects.  All objects have to share a common superclass and subclasses
cannot add any additional instance slots.

This is an alpha release.  Everything might change.  You probably
shouldn't use this in your own packages yet.  I am releasing this
now anyway because it is needed by [Epkg], for which it was written.
Documentation will be written once the API has been stabilized.

[eieio]:   https://www.gnu.org/software/emacs/manual/html_node/eieio/index.html
[emacsql]: https://github.com/skeeto/emacsql
[sqlite]:  https://www.sqlite.org
[epkg]:    https://gitlab.com/tarsius/epkg
