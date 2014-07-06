(ert-deftest snakehump--split-names-t ()
  (let ((expected '("foo" "bar" "baz")))
    (dolist (string '("foo_bar_baz" "foo-bar-baz" "fooBarBaz"
		      "FooBarBaz" "Foo::Bar::Baz"))
      (should (equal (snakehump--split-name string) expected)) )))

;;; Formats
(ert-deftest snakehump-drom-t ()
  (should (equal (snakehump-drom "foo-bar-baz") "fooBarBaz"))
  (should (equal (snakehump-drom "foo")         "foo"))
)

(ert-deftest snakehump-camel-t ()
  (should (equal (snakehump-camel "foo-bar-baz") "FooBarBaz"))
  (should (equal (snakehump-camel "foo")         "Foo"))
)

(ert-deftest snakehump-snake-t ()
  (should (equal (snakehump-snake "foo-bar-baz") "foo_bar_baz"))
  (should (equal (snakehump-snake "foo")         "foo"))
)

(ert-deftest snakehump-dash-t ()
  (should (equal (snakehump-dash "foo-bar-baz") "foo-bar-baz"))
  (should (equal (snakehump-dash "foo")         "foo"))
)

(ert-deftest snakehump-colon-t ()
  (should (equal (snakehump-colon "foo-bar-baz") "Foo::Bar::Baz"))
  (should (equal (snakehump-colon "foo")         "Foo"))
)


;;; Predicates
(ert-deftest snakehump-snake-p-t ()
  (should (equal (snakehump-snake-p "foo_bar_baz")   t))
  (should (equal (snakehump-snake-p "foo-bar-baz")   nil))
  (should (equal (snakehump-snake-p "Foo::Bar::Baz") nil))
  (should (equal (snakehump-snake-p "FooBarBaz")     nil))
  (should (equal (snakehump-snake-p "fooBarBaz")     nil))
)

(ert-deftest snakehump-dash-p-t ()
  (should (equal (snakehump-dash-p "foo_bar_baz")   nil))
  (should (equal (snakehump-dash-p "foo-bar-baz")   t))
  (should (equal (snakehump-dash-p "Foo::Bar::Baz") nil))
  (should (equal (snakehump-dash-p "FooBarBaz")     nil))
  (should (equal (snakehump-dash-p "fooBarBaz")     nil))
)

(ert-deftest snakehump-colon-p-t ()
  (should (equal (snakehump-colon-p "foo_bar_baz")   nil))
  (should (equal (snakehump-colon-p "foo-bar-baz")   nil))
  (should (equal (snakehump-colon-p "Foo::Bar::Baz") t))
  (should (equal (snakehump-colon-p "FooBarBaz")     nil))
  (should (equal (snakehump-colon-p "fooBarBaz")     nil))
)

(ert-deftest snakehump-camel-p-t ()
  (should (equal (snakehump-camel-p "foo_bar_baz")   nil))
  (should (equal (snakehump-camel-p "foo-bar-baz")   nil))
  (should (equal (snakehump-camel-p "Foo::Bar::Baz") nil))
  (should (equal (snakehump-camel-p "FooBarBaz")     t))
  (should (equal (snakehump-camel-p "fooBarBaz")     nil))
)

(ert-deftest snakehump-drom-p-t ()
  (should (equal (snakehump-drom-p "foo_bar_baz")   nil))
  (should (equal (snakehump-drom-p "foo-bar-baz")   nil))
  (should (equal (snakehump-drom-p "Foo::Bar::Baz") nil))
  (should (equal (snakehump-drom-p "FooBarBaz")     nil))
  (should (equal (snakehump-drom-p "fooBarBaz")     t))
)


;;; Query
(ert-deftest snakehump-current-format-t ()
  (should (equal (snakehump-current-format "foo_bar_baz")   'snake))
  (should (equal (snakehump-current-format "foo-bar-baz")   'dash))
  (should (equal (snakehump-current-format "Foo::Bar::Baz") 'colon))
  (should (equal (snakehump-current-format "FooBarBaz")     'camel))
  (should (equal (snakehump-current-format "fooBarBaz")     'drom))
)

;; Format
(ert-deftest snakehump-format-t ()
  (should (equal (snakehump-format "foo-bar-baz" 'snake) "foo_bar_baz"))
  (should (equal (snakehump-format "foo-bar-baz" 'dash)  "foo-bar-baz"))
  (should (equal (snakehump-format "foo-bar-baz" 'colon) "Foo::Bar::Baz"))
  (should (equal (snakehump-format "foo-bar-baz" 'camel) "FooBarBaz"))
  (should (equal (snakehump-format "foo-bar-baz" 'drom)  "fooBarBaz"))
)


;;; List / Cycle
(ert-deftest snakehump--list-next-t ()
  (should (equal (snakehump--list-next 'a '(a b c d)) 'b))
  (should (equal (snakehump--list-next 'd '(a b c d)) nil))
)

(ert-deftest snakehump--cycle-next-t ()
  (should (equal (snakehump--cycle-next 'a '(a b c d)) 'b))
  (should (equal (snakehump--cycle-next 'd '(a b c d)) 'a))
  (should (equal (snakehump--cycle-next 'x '(a b c d)) 'a))
)

(ert-deftest snakehump--list-prev-t ()
  (should (equal (snakehump--list-prev 'a '(a b c d)) nil))
  (should (equal (snakehump--list-prev 'd '(a b c d)) 'c))
)

(ert-deftest snakehump--cycle-prev-t ()
  (should (equal (snakehump--cycle-prev 'a '(a b c d)) 'd))
  (should (equal (snakehump--cycle-prev 'd '(a b c d)) 'c))
  (should (equal (snakehump--cycle-prev 'x '(a b c d)) 'd))
)

;;; Next / Prev
(ert-deftest snakehump-current-format-t ()
  (let ((snakehump-hump-cycle '(snake dash colon camel drom)))
    (should (equal (snakehump-next "foo_bar_baz"  ) "foo-bar-baz"  ))
    (should (equal (snakehump-next "foo-bar-baz"  ) "Foo::Bar::Baz"))
    (should (equal (snakehump-next "Foo::Bar::Baz") "FooBarBaz"    ))
    (should (equal (snakehump-next "FooBarBaz"    ) "fooBarBaz"    ))
    (should (equal (snakehump-next "fooBarBaz"    ) "foo_bar_baz"  ))
    )
)

(ert-deftest snakehump-current-format-custom-t ()
  (let ((snakehump-hump-cycle '(drom camel colon dash snake)))
    (should (equal (snakehump-next "fooBarBaz"    ) "FooBarBaz"    ))
    (should (equal (snakehump-next "FooBarBaz"    ) "Foo::Bar::Baz"))
    (should (equal (snakehump-next "Foo::Bar::Baz") "foo-bar-baz"  ))
    (should (equal (snakehump-next "foo-bar-baz"  ) "foo_bar_baz"  ))
    (should (equal (snakehump-next "foo_bar_baz"  ) "fooBarBaz"))
    )
)

(ert-deftest snakehump-xcurrent-format-custom-restrict-t ()
  (let ((snakehump-hump-cycle '(camel dash)))
    (should (equal (snakehump-next "fooBarBaz"    ) "FooBarBaz"))
    (should (equal (snakehump-next "FooBarBaz"    ) "foo-bar-baz"))
    (should (equal (snakehump-next "Foo::Bar::Baz") "FooBarBaz"))
    (should (equal (snakehump-next "foo-bar-baz"  ) "FooBarBaz"))
    (should (equal (snakehump-next "foo_bar_baz"  ) "FooBarBaz"))
    )
)
