# Performance

I've implemented the same feature within one of our application using
[tomjkidd/elm-multiway-tree-zipper](http://package.elm-lang.org/packages/tomjkidd/elm-multiway-tree-zipper/latest)
and this package and profiled performance with real production data.

Application fetch data from two endpoints and then builds a Tree from them. The total number of nodes within resulting three is 4278.

# Strict Implementation (tomjkidd/elm-multiway-tree-zipper)

MacOS Chrome:
![](mac_strict.png)

Arch Linux Chromium:

# HW specifications

MacOS Chrome:
![](mac_lazy.png)

Arch Linux Chromium:

## MacOS Chrome

```
MacBook Pro (Retina, 15-inch, Mid 2015)
2,2 GHz Intel Core i7
16 GB 1600 MHz DDR3
Intel Iris Pro 1536 MB
```

```
Chrome Version 63.0.3239.108 (Official Build) (64-bit)
```


## Arch Linux Chromium
