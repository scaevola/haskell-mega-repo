# fum-carbon

Successor of [FUM5](https://github.com/futurice/futurice-ldap-user-manager)

/Carbon is the 6th element/.

Brief feature list:
- User accounts
    - Login information
    - Email aliases
    - SSH keys
    - Picture
- Group management


## Architecture

TBW

For terminology watch [GOTO 2017 • The Many Meanings of Event-Driven Architecture • Martin Fowler](https://www.youtube.com/watch?v=STKCRSUsyP0)

## Bootstrapping

*FUM* uses own data for access control, therefore bootstrapping could be a challenge, but it isn't.

When pristine instance is booted, the *forbidden page* has a form to perform a
bootstrap command, which will create a sudoers group and a user, adding the
user to the group.

## Adding command

See commit
https://github.com/futurice/haskell-mega-repo/pull/462/commits/8d62cfce001a427b049821abac82db86817d2ff7
for an example. In short: add a type implementing `Command` class, and add it to the list.
