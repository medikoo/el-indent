# el-indent

An `indent-line-function` configurator

## About

el-indent allows to quckly build an `indent-line-function` for a major
mode. It may be helpful if you'd like to customize the way code is
indented in mode you're using or if you're writing support for a new
mode.

## Installation

- There is a dependency on [el-kit](https://github.com/medikoo/el-kit), you will need to place the `el-kit` .el files into your `.emacs` `load-path`. (I'd recommend putting them in their own folder.)

- Add `el-indent.el` to you `.emacs` `load-path`

## Usage

### How to use ready configurations ?

Load feature and add hook to your mode, e.g. for lisp-mode:

```
(require 'el-indent/lisp)
(add-hook 'lisp-mode-hook 'el-indent-set-lisp)
```

Naming convention for other modes stays the same. For `xml-mode` will be:
`el-indent/xml` and `el-indent-set-xml` etc.

### How to configure your own indent function ?

To configure `indent-line-function` you need to provide list of strings that
affects indentation.

Example - let's say we deal with mode in which code is nested by parenthesis.
So `(` nests **+1** and `)` nests **-1**, we configure rules this way:

```
(list
    (list "\(" "\)")
    (list 1    -1)
    (list t    t))
```

In first list you group strings (in form of regexps) that affect
indentation. In second you say which way it affects indentation
and in third list you say what rules should be followed after that string in
code occured. If it's same rules then you may just put t. otherwise you
should put object with differnt rules that would be used afterwards.

Sometimes in first group you'll put string that change the rules
of indentation but actually doesn't indent the code. For example beginning of
comments might be the case, then just put 0 in second list.

Finally you'll pack rules into functions for direct use:

```
(setq el-indent-exp-mymode
    (el-indent-build-exps
        (list
            (list "\(" "\)")
            (list 1    -1)
            (list t    t))))

; el-indent-build-exps translates your rules into expressions that are
evaluated when actually indenting.

(defun el-indent-mymode ()
    (el-indent-line (lambda () el-indent-exp-mymode)))

; In place of lambda you may put your custom function that will return different
; expressions according to place in which indent is calculated

(defun el-indent-set-mymode ()
    (setq indent-line-function 'el-indent-mymode))
```

That's it, configuration may be used as described in [**How to use ready configurations**.](#how-to-use-ready-configurations-)

Indent rules are generally more complicated than in this example. You
can learn more from configurations that are included with this
project.
