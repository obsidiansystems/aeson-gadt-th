# Revision history for aeson-gadt-th

## 0.2.1.2

* Add version bounds to cabal file

## 0.2.1.1

* Drop markdown-unlit in favor of "Bird"-style LHS to avoid some cross-compilation issues.

## 0.2.1.0

* Extend type variable substitution to handle all current cases in template-haskell.
* Better deal with data constructors having an index that is polymorphic, but can be determined from the other type parameters.
* Handle data constructors that are constrained by type classes.

## 0.2.0.0

* Add changelog
* Add option to modify constructor tag in derived JSON
* Add test suite
