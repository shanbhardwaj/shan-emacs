# Mail setup notes (mu4e)

How the email system is wired together, plus a day-to-day cheatsheet and a
troubleshooting runbook. Config lives in `lisp/init-email.el`; the dashboard
layout in `mu4e-dashboard.org`.

## Moving parts

- **mbsync (isync)** fetches mail for all four accounts into `~/Mail/`.
  Runs every 300s via `brew services` (launchd job `homebrew.mxcl.isync`).
  Config: `~/.mbsyncrc`. Output goes to /dev/null, so failures are silent.
- **mu** indexes `~/Mail`; mu4e reindexes every 300s (it does not fetch).
- **msmtp** sends mail; picks the account by the From header.
  Config: `~/.msmtprc`, log: `~/Mail/msmtp.log`.
- **Passwords** are Google app passwords stored in the macOS Keychain:

  | account   | service        | keychain account (-a)      |
  |-----------|----------------|----------------------------|
  | Addval    | mu4e-addval    | shan@addvalsolutions.com   |
  | Kulcare   | mu4e-kulcare   | shantanu@kulcare.com       |
  | Codetiger | mu4e-codetiger | shan@codetiger.com         |
  | Gmail     | mu4e-gmail     | bhardwaj.10                |

  `.mbsyncrc` and `.msmtprc` must use these exact service/account strings.

## Day-to-day keybindings

- `C-c m` mu4e main screen (bookmarks with live counts), `C-c M` dashboard
- Bookmarks: `b u` unread, `b i` all inboxes, `b f` reply queue (flagged),
  `b t` today, `b y` yesterday; per-account `b k/a/g/c`
- `j` jump to a maildir, `s` search, `/` narrow results, `^` back to main
- Compose: `R` reply, `W` reply-all, `F` forward, `C` new;
  `C-c C-c` send, `C-c C-k` discard, `C-c C-a` attach
- Triage (mark, then `x` to execute): `r` refile/archive, `d` trash,
  `m` move, `+`/`-` flag/unflag, `!`/`?` read/unread
- View: `e` save attachments, `a` actions (e.g. open in browser)
- Reply queue workflow: `+` to enqueue while triaging, `b f` to work the
  queue, `-` after replying.

## Flags column

Uppercase = state, lowercase = property:
`N` new, `S` seen, `R` replied, `P` forwarded, `F` flagged (reply queue),
`D` draft, `T` trashed | `a` attachment, `l` mailing list, `p` addressed to
me personally, `x` encrypted, `s` signed, `c` calendar invite.

Trash/Spam mail is hidden from all searches unless the query itself names
those folders (see `mu4e-search-hide-predicate` in init-email.el).

## Troubleshooting

**Sending fails with exit code 77 / no new mail arriving.**
Almost always an expired/revoked Google app password (Google kills them on
password changes). Check which account:

    for ch in Gmail Addval Kulcare Codetiger; do
      mbsync -l "$ch" >/dev/null 2>&1 || echo "$ch broken"
      mbsync -l "$ch" 2>&1 | grep -q AUTHENTICATIONFAILED && echo "$ch auth failed"
    done
    tail -3 ~/Mail/msmtp.log   # shows SMTP-side errors

Fix: generate a new app password at https://myaccount.google.com/apppasswords
(signed in as the RIGHT account), then store it with NO SPACES:

    security add-generic-password -U -s mu4e-<acct> -a "<keychain account>" -w "16charsnospaces"

**"PassCmd exited with status 44"** — keychain item not found: the -s/-a
strings in .mbsyncrc/.msmtprc don't match the keychain entry.

**"Maildir error: duplicate UID N"** — leftover from an interrupted sync.
Find the two files with `,U=N:` in that folder, keep the older one, rename
the newer to drop the `,U=N` part. Stop the service first
(`brew services stop isync`), sync manually, restart it.

**"channel is locked"** — a manual `mbsync -a` raced the launchd job.
Stop the service before manual syncs.

**Index out of date** — mu4e reindexes on its 300s timer; `S` in mu4e
forces it. `mu index` from a shell only works when mu4e isn't running.
