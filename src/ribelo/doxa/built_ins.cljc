(ns ribelo.doxa.built-ins)


(def query-fns
  {'= =, '== ==, 'not= not=, '!= not=, '< <, '> >, '<= <=, '>= >=, '+ +, '- -,
   '* *, '/ /, 'quot quot, 'rem rem, 'mod mod, 'inc inc, 'dec dec, 'max max, 'min min,
   'zero? zero?, 'pos? pos?, 'neg? neg?, 'even? even?, 'odd? odd?, 'compare compare,
   'rand rand, 'rand-int rand-int,
   'true? true?, 'false? false?, 'nil? nil?, 'some? some?, 'not not, 'and and-fn, 'or or-fn,
   'complement complement, 'identical? identical?,
   'identity identity, 'keyword keyword, 'meta meta, 'name name, 'namespace namespace, 'type type,
   'vector vector, 'list list, 'set set, 'hash-map hash-map, 'array-map array-map,
   'count count, 'range range, 'not-empty not-empty, 'empty? empty?, 'contains? contains?,
   'str str, 'pr-str pr-str, 'print-str print-str, 'println-str println-str, 'prn-str prn-str, 'subs subs,
   're-find re-find, 're-matches re-matches, 're-seq re-seq, 're-pattern re-pattern,
   'ground identity,
   'tuple vector, 'untuple identity
   })
