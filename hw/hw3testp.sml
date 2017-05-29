val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test101 = only_capitals ["a","B","C"] = ["B","C"]

val test102 = only_capitals ["Abc","ABc","abC"] = ["Abc","ABc"]

val test103 = only_capitals ["1AB","?AB","Abc","ABc","abC"] = ["Abc","ABc"]

val test2 = longest_string1 ["A","bc","C"] = "bc"

val test201 = longest_string1 ["A","bc","C", "de"] = "bc"

val test202 = longest_string1 ["A","bc","C", "def"] = "def"

val test3 = longest_string2 ["A","bc","C"] = "bc"

val test301 = longest_string2 ["A","bc","C", "de"] = "de"

val test302 = longest_string2 ["A","bc","C", "def"] = "def"

val test4a = longest_string3 ["A","bc","C"] = "bc"

val test4a1 = longest_string3 ["A","bc","C", "de"] = "bc"

val test4a2 = longest_string3 ["A","bc","C", "def"] = "def"

val test4b = longest_string4 ["A","B","C"] = "C"

val test4b1 = longest_string4 ["A","bc","C", "de"] = "de"

val test4b2 = longest_string4 ["A","bc","C", "def"] = "def"
