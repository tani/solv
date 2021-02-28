# Sol V

IPC/CPC theorem prover in Prolog. This software is based on tableaux algorithm, which is a naive implementation.
Thus, it might be slow if we input the large proposition.

## Example

```
$ solv ipc 'imply(p, not(not(p)))'
unprobable
$ echo $? # Exit status
1
$ solv cpc 'imply(p, not(not(p)))'
probable
$ echo $? # Exit status
0
```

## Q&A

- Why is the name "Sol V"?
  - Because this software **SOLV**es satisfiability problem in IPC/CPC.
- Do you know alternatives?
  - Yes, I do. You can find it like [IPC Solver](https://github.com/qnighy/ipc_solver).

## Copyright and license
Copyright 2021 TANIGUCHI Masaya. All rights reserved.

This work is licensed under the GPLv3 or later.
