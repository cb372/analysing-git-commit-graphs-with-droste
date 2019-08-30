# JGit + droste = ???

Messing around with using [droste](https://github.com/higherkindness/droste) to
do simple analysis on a git commit graph.

Using [JGit](https://wiki.eclipse.org/JGit/User_Guide) for the actual git stuff,
then wrapping its mutable iterator-style API into a recursive data structure
suitable for use with recursion schemes.

None of this stuff is actually useful, and it could all be done trivially with
JGit alone. It's just an attempt to get my head around the basics of droste and
recursion schemes.
