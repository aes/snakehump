snakehump
=========

This provides convenient format cycling functions for different compound word
conventions.

for example, (with the cursor anywhere in the word), this will cycle through:

    foo_bar_baz
    foo-bar-baz
    fooBarBaz
    FooBarBaz
    Foo::Bar::Baz

(For reference, that's `snake`, `dash`, `drom`, `camel` and `colon`.)

Which of these are active can be customized in `snakehump-hump-cycle`.

Words that are not in any of the `snakehump-hump-cycle` when reformatting is
requested will be normalized to the first format in the list.
