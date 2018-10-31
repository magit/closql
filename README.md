Store EIEIO objects using EmacSQL
=================================

Store uniform [EIEIO] objects in an [EmacSQL] database.  [SQLite] is
used as backend.  This library imposes some restrictions on what kind
of objects can be stored; it isn't intended to store arbitrary
objects.  All objects have to share a common superclass and subclasses
cannot add any additional instance slots.

[eieio]:   https://www.gnu.org/software/emacs/manual/html_node/eieio/index.html
[emacsql]: https://github.com/skeeto/emacsql
[sqlite]:  https://www.sqlite.org
[epkg]:    https://github.com/emacscollective/epkg
