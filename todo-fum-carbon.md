> Brief HOWTO:
> - Lines starting with > are comments
> - B:N:W is best:nominal:worst estimations. 5%/50%/95% confidence levels.
>  Some task can be done in one day, two days should be enough, but if
>   everything goes wrong, it will take five days: 1/2/5
> - Lines starting with # and ## are headers, so you can group the tasks
> - Empty lines are ignored
> - The format makes file reasonable looking MarkDown file.

> B:N:W  Task

# Employee

- 1:1:2 Editing employee fields (status)
- 1:2:5 Picture upload
- 1:2:2 Adding mail aliases
- 1:2:2 Password change
- 1:2:2 SSH keys processing
- 1:1:1 view tweak: github, flowdock, internal/external, contractEndDate

# Groups

- 1:2:4 Views
- 1:1:3 Special groups
- ~1:1:2 CreateGroup command~
- 1:1:2 EditGroup command
- ~1:1:2 AddEmployeeToGroup~
- 2:3:4 group edititng acl

# Customer

- 10:15:20 Everything

# Shared mailboxes

- 1:2:4 Views
- 1:1:2 CreateMailbox command
- 1:1:2 EditMailbox command
- 1:1:2 AddEmployeeToMailbox command
- 2:3:4 mailbox editing acl?

# Technicals

- ~1:1:1 Bootstrap~

## Event stream

- ~2:2:4 Provide internal way to process events. Needed for incremental~
- 1:1:2 Websocket interface

## LDAP Integration

- 3:5:7 Full sync
- 3:5:7 Incremental "instant" sync for e.g. password changes

## Transactor

- ~1:2:2 Transactor thread~
- ~1:2:2 Persistence~

# Personio -> PlanMill

- ~2:3:4 GitHub prototype~
- 1:1:1 GitHub prototype completion (blocked on Personio data mostly)
- 10:15:20 Raw estimate
