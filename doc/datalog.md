`datalog` defacto has become the standard way of searching db in `clojure`. the api is no different from that available in `datascript`, any exception should be considered as a bug. query is a simple bruteforce however, however all loops are as tight as possible and `clojure/script` `protocols` or `java` `interfaces` are used directly. the query is &ldquo;compiled&rdquo; in the manner seen in [malli](https://github.com/metosin/malli/tree/master/src/malli). the implementation of `comp` and `some/every-pred` is shamelessly ripped off.

this allows a truly surprising performance with a very simple design and without any serious query engine. for most queries excluding this with multiple joins, `doxa` is the fastest available db for `clojurescript`.

at the moment `doxa` does not support nested `or` and `and`, but implementation is planned when i figure out how (:
