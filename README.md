# hextream

This library provides Haskell-native, streaming-friendly XML parsers.

Parsers are organized into 3 layers of increasing abstraction level: low, middle and high. Applications will most probably want to rely on the high-level layer.

The library does not define a data type nor parser for a whole XML document. Instead, parsers operate on XML tokens of various sizes, to allow for a usage in a streaming context (e.g. using [`streaming-attoparsec`](https://hackage.haskell.org/package/streaming-attoparsec), [`pipes-attoparsec`](https://hackage.haskell.org/package/pipes-attoparsec) or [`conduit-attoparsec`]()).

The implementation is based on the following specifications:
- [Extensible Markup Language (XML) 1.0 (Fifth Edition)](https://www.w3.org/TR/REC-xml/)
- [Namespaces in XML 1.0 (Third Edition)](https://www.w3.org/TR/xml-names/)


## Caveats

The following parts of the XML standard are not supported:
- [external entities](https://www.w3.org/TR/REC-xml/#sec-external-ent)
- [parameter entities](https://www.w3.org/TR/REC-xml/#dt-PE)
- the doctype [internal subset](https://www.w3.org/TR/REC-xml/#NT-intSubset), except for [general entity declarations](https://www.w3.org/TR/REC-xml/#NT-GEDecl)


