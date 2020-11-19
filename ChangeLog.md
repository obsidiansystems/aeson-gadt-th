# Revision history for aeson-gadt-th

## 0.2.5.0

* Support for GHC 8.10
* Support for aeson 1.5.*
* Fix [#21](https://github.com/obsidiansystems/aeson-gadt-th/issues/21): deriveJSONGADT requires `toJSON` and `parseJSON` to be in scope
* Fix [#25](https://github.com/obsidiansystems/aeson-gadt-th/issues/25): Test suite does not compile (on GHC 8.10)

## 0.2.4

* Support for GHC 8.8

## 0.2.2

* Do a better job determining which variables are rigid when looking for instances
* Unify discovered instance head with argument type and make the same substitution in the context that constrains the instance we're writing

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
