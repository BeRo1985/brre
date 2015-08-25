
## For my new regular expression engine see [FLRE](https://github.com/BeRo1985/flre) which is preferred now over BRRE, since FLCE with its better structured code is more easily maintainable and therefore also overall less error-prone regarding bugs. But FLRE doesn't support irregular expression features like backreferences, lookaheads, lookbehinds and so on, if you do need these, then BRRE will be more preferred, but please, disable the buggy DFA stuff at BRRE then.

BRRE ( **B** e **R** o **R** egularExpression **E** ngine) is a fast, safe and efficient regular expression library, which is implemented in Object Pascal (Delphi and Free Pascal) but which is even usable from other languages like C/C++ and so on. 

It implements almost all the common Perl and PCRE features and syntactic sugars. It also finds the leftmost-first match, the same match that Perl and PCRE would, and can return submatch information, and has support for backtracking stuff as backreferences and so on. But it also features a modifier flag for a yet experimental POSIX-style leftmost-longest match behaviour mode. 

BRRE is licensed under the LGPL v2.1 with static-linking-exception.

The sercet of BRRE speed is that BRRE uses multiple subengines with automatic selection at the regex-compiling-process:

* **Fixed string search** for pure static literal string regexs, shift-or for short strings shorter than 32 chars, and boyer-moore for strings longer than or equal 32 chars.
* **On-the-fly-computed DFA** (aka lazy DFA, DFA with caching of parallel-threaded-NFA/Thompson-NFA states) to find the whole match boundaries. For submatches, if they exists, it uses the remain subengines after the DFA match pass. This subengine is very fast. *New:* Backtracking-only features like backreferences, lookahead, lookbehind, recursive patterns, callouts etc. are in the DFA pass, depending on each backtracking-only instruction, dummy no-op or zero-or-more-catch-all-wildcard operations, so that the DFA pass can also react as an approximately-match-begin prefilter (but not match-end, because only the backtracker can find the true end of a so such match) for the following backtracking pass as end pass. 
* **Onepass NFA** This subengine is quite still fast. But it can process simple regexs only, where it's always immediately obvious when a repetition ends. For example `x(y|z)` and `x*yx*` are onepass, but `(xy)|(xz)` and `x*x*` not, but the regex abstract syntax tree optimizer optimizes the most cases anyway out, before the bytecode and the OnePassNFA state map will generated. The base idea is from the re2 regex engine from Google.
* **Bitstate NFA** This subengine is quite still fast. But it can process short regexs only with less than 512 regex VM bytecode instructions. It's a backtracker with a manual stack in general, but it members the already visited (state, string position) pairs in a bitmap. The base idea is even from the re2 regex engine from Google.
* **Parallel threaded NFA** (aka Thompson NFA). This subengine supports the most 08/15 regular expression syntax features _except_ backtracking-stuff as backreferences and so on. And this subengine is also still fast, but not so fast like the onepass NFA subengine.
* **Backtracker** This subengine is for all cases, for whose the other subengines can't handle these, for example regexs with backreferences stuff and so on. It uses mainly a manual stack, but it uses the native stack only for some few special regex VM bytecode instructions. And it can fallback into FixedStringSearch/OnePassNFA/BitstateNFA/ParallelNFA (but not DFA, because it's only for whole match boundaries) for sub paren regions of a regexp again with a **yet highly experimental** optional modifier flag.

All these subengines are UTF8 and bytewise-latin1 capable at the same time. And BRRE has Unicode 6.1.0 support at the UTF8 work mode. It's with double matchcapturing-bookkeeping, it captures always the code unit offsets and code point offsets at the same time, to save costly UTF8 Code Point to and from Byte Code Unit reindexing time, for example at the backtracking features. The UTF8 decoder of BRRE is itself a small DFA.

And as an addon, BRRE features prefix presearching with shift-or for short strings shorter than 32 chars, and boyer-moore (or naive bruteforce if UTF8) for strings longer than or equal 32 chars. So for example the prefix for the example regex `{{{/\bHel(?:lo|loop) [A-Za-z]+\b/}}}` is `Hello`.

And BRRE can process 0-based null terminated C/C++ and 1-based (Object-)Pascal strings, so it has also a foreign API for usage with C/C++ (see BRRELib.dpr).

BRRE features a prefilter boolean expression string generation feature in two variants, once as simple variant and once as SQL variant. For example, BRRE converts `{{{/(hello|hi) world[a-z]+and you/PO}}}` into the prefilter boolean expression string `{{{("hello world" OR "hi world") AND "and you"}}}` and into the prefilter boolean short expression string `{{{("hello world"|"hi world")and you}}}` and into the prefilter SQL boolean full text search expression string `{{{+("hello world" "hi world") +("and you")}}}` and into the prefilter boolean SQL expression string `{{{(((field LIKE "%hello world%") OR (field like "%hi world%")) AND (field like "%and you%"))}}}` where the field name is freely choosable, and BRRE converts `{{{/(hello|hi) world[a-z]+and you/P}}}` without O modifier flag into the prefilter boolean expression string `{{{("hello world" OR "hi world") AND * AND "and you"}}}` and into the prefilter boolean short expression string `{{{(hello world|hi world)*and you}}}`, so with wildcards then now. This feature can reduce the number of actual regular expression searches significantly, if you combine it with the data storage on the upper level (for example with with a text trigram index). 

For a first-try-performance-comparsion see [Benchmark](https://code.google.com/p/brre/wiki/Benchmark)

IRC: #brre on freenode

<a href="http://flattr.com/thing/504346/BRRE-yet-another-efficient-principled-regular-expression-library" target="_blank">
<img src="http://api.flattr.com/button/flattr-badge-large.png" alt="Flattr this" title="Flattr this" border="0" /></a>
