## Dependencies

* oasis (for compilation)
* menhir
* sedlex

## Preparation

```
opam install oasis
opam install sedlex
opam install menhir
```

## Compilation

```
oasis setup
make build install
```

## Usage

The most common command is

```
hxparser --json --print-reject YourFile.hx
```

You can also specify a directory, in which case all .hx file are recursively parsed.

## Questions

> Why?

We are slowly heading towards Haxe 4, so the idea came up to rewrite the parser in a more maintainable way with a strong focus on IDE support. This project is the beginning of said undertaking and, if successful, will be integrated as the official Haxe parser.

> Performance?

Performance is not that great at the moment, roughly 50% slower than the current Haxe parser. We are going to investigate and improve on this.

> I'm a retarded Windows user, how do I opam?

Check out our [Building on Windows (mingw)](http://haxe.org/documentation/introduction/building-haxe.html) instructions for Haxe. You might have to run the `opam install` commands in a cygwin window, but everything else should work fine.

> hxparse, haxeparser, hxparser... Are you serious?

Sorry!

> What's the difference to the current Haxe parser?

The current parser is a recursive-descent parser which is implemented using the camlp4o extension to OCaml. It has display support built-in, which is convenient for simple cases but not very flexible overall.

The new parser uses a [yacc-like grammar definition file](src/syntax/parser.mly), which is much more concise. Parser resuming is to be built into the [parser loop](src/syntax/parserDriver.ml) instead, which allows for a better separation and more flexibility.

Furthermore, it uses [sedlex](https://github.com/alainfrisch/sedlex) which supports Unicode lexers.

> Any problems?

While developing this parser, the author came to the realization that the Haxe grammar is in the LL(2) category due to a specific construct:

```haxe
{
	if (cond)
		e1;
	else
		e3;
}
```

After parsing `e1`, the current parser looks ahead for 2 tokens to see if there's `; else` coming up. Menhir supports LR(1) grammars, which required a very ugly workaround in the parser loop.

Another problem is that Haxe allows some optional `;` if they are preceded by a `}` token:

```haxe
{
	var a = { }; // optional ;
	b;
}
```

This seems to be tricky to express in the grammar because if the `;` is omitted, the `}` serves double-purpose: As termination token of the object declaration (or whatever is to be closed) and the block-element.

It remains to be seen if these are actual issues or if the author is just incompetent.

> Any good news?

It successfully parses my GitHub directory recursively and doesn't use much memory.